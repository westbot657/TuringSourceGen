using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;


namespace TuringSourceGen;

[Generator]
public class WasmRsIncrementalSourceGenerator : IIncrementalGenerator
{

    private static readonly Dictionary<string, (string opposite, MethodData converterInfo)> ConversionTypes = new();
    
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        interopBinders.Clear();
        var rustClassProvider = context.SyntaxProvider
            .CreateSyntaxProvider(
                (s, _) => s is ClassDeclarationSyntax,
                (ctx, _) => GetClassDeclarationForSourceGen(ctx))
            .Where(t => t.isAnnotatedClass)
            .Select((t, _) => t.Item1);
        
        context.RegisterSourceOutput(context.CompilationProvider.Combine(rustClassProvider.Collect()),
            (ctx, t) => GenerateCode(ctx, t.Left, t.Right));
    }

    private static (ClassDeclarationSyntax, bool isAnnotatedClass) GetClassDeclarationForSourceGen(
        GeneratorSyntaxContext context)
    {
        var classDeclarationSyntax = (ClassDeclarationSyntax)context.Node;

        foreach (var attributeSyntax in classDeclarationSyntax.AttributeLists.SelectMany(attributeListSyntax => attributeListSyntax.Attributes))
        {
            if (context.SemanticModel.GetSymbolInfo(attributeSyntax).Symbol is not IMethodSymbol attributeSymbol)
                continue;

            var attributeName = attributeSymbol.ContainingType.ToDisplayString();

            if (attributeName is "Turing.Interop.RustClass" or "Turing.Interop.CodecClass" or "Turing.Interop.InteropClass")
                return (classDeclarationSyntax, true);
        }

        return (classDeclarationSyntax, false);
    }

    private class FieldData
    {
        public IFieldSymbol? Field { get; set; }
        public string FieldName { get; set; } = "";
        public string Type { get; set; } = "";
        public bool IsStatic { get; set; }
    }

    private class ParameterData
    {
        public IParameterSymbol? Parameter { get; set; }
        public string Name { get; set; } = "";
        public string Type { get; set; } = "";
    }

    private class MethodData
    {
        public IMethodSymbol? MethodSymbol { get; set; }
        public string Name { get; set; } = "";
        public string ReturnType { get; set; } = "";
        public bool IsStatic { get; set; }
        public List<ParameterData> Parameters { get; set; } = [];
    }
    
    private void GenerateCode(SourceProductionContext context, Compilation compilation,
        ImmutableArray<ClassDeclarationSyntax> _classDeclarations)
    {
        var classDeclarations = _classDeclarations.ToList();
        // first, search for codec, since it has to be constructed before anything else
        foreach (var classDeclarationSyntax in classDeclarations)
        {

            // We need to get semantic model of the class to retrieve metadata.
            var semanticModel = compilation.GetSemanticModel(classDeclarationSyntax.SyntaxTree);

            // Symbols allow us to get the compile-time information.
            if (semanticModel.GetDeclaredSymbol(classDeclarationSyntax) is not INamedTypeSymbol classSymbol)
                continue;

            var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();

            var className = classDeclarationSyntax.Identifier.Text;

            if (!classSymbol.GetAttributes()
                    .Any(a => a.AttributeClass?.ToDisplayString() == "Turing.Interop.CodecClass")) continue;
            BuildCodec(context, classSymbol, namespaceName, className, classDeclarations, compilation);
            break;
        }



        // then build things that rely on the codec
        foreach (var classDeclarationSyntax in classDeclarations)
        {
            
            // We need to get semantic model of the class to retrieve metadata.
            var semanticModel = compilation.GetSemanticModel(classDeclarationSyntax.SyntaxTree);

            // Symbols allow us to get the compile-time information.
            if (semanticModel.GetDeclaredSymbol(classDeclarationSyntax) is not INamedTypeSymbol classSymbol)
                continue;

            var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();

            var className = classDeclarationSyntax.Identifier.Text;

            if (!classSymbol.GetAttributes()
                    .Any(a => a.AttributeClass?.ToDisplayString() == "Turing.Interop.RustClass")) continue;
            
            var wrapped = classSymbol.GetMembers()
                .OfType<IFieldSymbol>()
                .Where(f => f.GetAttributes().Any(a => a.AttributeClass?.ToDisplayString() == "Turing.Interop.RustWrapped"))
                .Select(f => new FieldData
                {
                    Field = f,
                    FieldName = f.Name,
                    Type = f.Type.ToDisplayString(),
                    IsStatic = f.IsStatic
                })
                .First();
            
            var fields = classSymbol.GetMembers()
                .OfType<IFieldSymbol>()
                .Where(f => f.GetAttributes().Any(a => a.AttributeClass?.ToDisplayString() == "Turing.Interop.RustField"))
                .Select(f => new FieldData
                {
                    Field = f,
                    FieldName = f.Name,
                    Type = f.Type.ToDisplayString(),
                    IsStatic = f.IsStatic
                })
                .ToList();

            var methods = classSymbol.GetMembers()
                .OfType<IMethodSymbol>()
                .Where(m => m.GetAttributes()
                    .Any(a => a.AttributeClass?.ToDisplayString() == "Turing.Interop.RustMethod"))
                .Select(m => new MethodData
                {
                    MethodSymbol = m,
                    Name = m.Name,
                    ReturnType = m.ReturnType.ToDisplayString(),
                    IsStatic = m.IsStatic,
                    Parameters = m.Parameters.Select(p => new ParameterData
                    {
                        Parameter = p,
                        Name = p.Name,
                        Type = p.Type.ToDisplayString(),
                    }).ToList()
                }).ToList();
            
            var codeBuilder = new StringBuilder();
            codeBuilder.AppendLine("// <auto-generated/>");
            codeBuilder.AppendLine("using System;");
            codeBuilder.AppendLine("using System.Runtime.InteropServices;");
            codeBuilder.AppendLine($"namespace {namespaceName} {{");
            
            ConstructSource(context, classSymbol, codeBuilder, className, wrapped, fields, methods);
            
            codeBuilder.AppendLine("}");

            
            var code = codeBuilder.ToString();
            // Add the source code to the compilation.
            context.AddSource($"{className}.g.cs", SourceText.From(code, Encoding.UTF8));
        }

        // and finally, build a function to call all the dll binding functions
        foreach (var classDeclarationSyntax in classDeclarations)
        {

            // We need to get semantic model of the class to retrieve metadata.
            var semanticModel = compilation.GetSemanticModel(classDeclarationSyntax.SyntaxTree);

            // Symbols allow us to get the compile-time information.
            if (semanticModel.GetDeclaredSymbol(classDeclarationSyntax) is not INamedTypeSymbol classSymbol)
                continue;

            var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();

            var className = classDeclarationSyntax.Identifier.Text;

            if (!classSymbol.GetAttributes()
                    .Any(a => a.AttributeClass?.ToDisplayString() == "Turing.Interop.InteropClass")) continue;
            BuildBindCalls(context, classSymbol, namespaceName, className);
            break;
        }
    }

    private const string PtrDeco = "[System.Runtime.InteropServices.UnmanagedFunctionPointer(System.Runtime.InteropServices.CallingConvention.Cdecl)]";


    private static HashSet<string> interopBinders = [];
    
    private class RustClassConstructor(string name)
    {
        private string Name { get; } = name;
        
        private List<string> methodDefs { get; } = [];
        
        private List<(string del, string func, string? rustName)> delegateNames { get; } = [];

        public void build(SourceProductionContext context, ITypeSymbol classSymbol, StringBuilder sb)
        {
            sb.AppendLine( "[StructLayout(LayoutKind.Sequential)]");
            sb.AppendLine($"public struct {Name}Rs");
            sb.AppendLine( "{");
            sb.AppendLine( "    public IntPtr ptr;");
            
            sb.AppendLine( "}");
            
            sb.AppendLine($"public partial class {Name} : IDisposable");
            sb.AppendLine( "{");

            foreach (var methodDef in methodDefs)
            {
                sb.AppendLine($"    {methodDef}");
            }
            
            interopBinders.Add($"{classSymbol.ToDisplayString()}.BindInteropFunctions();");
            sb.AppendLine("    private static System.Collections.Generic.List<Delegate> _keepAlive = new System.Collections.Generic.List<Delegate>();");
            sb.AppendLine("    public static void BindInteropFunctions()");
            sb.AppendLine("    {");

            var useVar = true;
            foreach (var delegateName in delegateNames)
            {
                var v = (useVar ? "var " : "");
                useVar = false;
                sb.AppendLine($"        {v}del = Delegate.CreateDelegate(typeof({delegateName.del}), typeof({Name}).GetMethod(\"{delegateName.func}\"));");
                sb.AppendLine("        _keepAlive.Add(del);");
                sb.AppendLine($"        {v}funcPtr = Marshal.GetFunctionPointerForDelegate(del);");
                sb.AppendLine($"        {v}namePtr = Marshal.StringToHGlobalAnsi({delegateName.rustName});");
                sb.AppendLine("        WasmInterop.register_function(namePtr, funcPtr);");
                sb.AppendLine("        Marshal.FreeHGlobal(namePtr);");
            } 
            
            sb.AppendLine("    }");
            
            sb.AppendLine( "}");
        }

        public void ProcessWrapped(SourceProductionContext context, string className, FieldData wrapped)
        {
            var sb = new StringBuilder();

            var attr = wrapped.Field?.GetAttributes().First(a => a.AttributeClass?.ToDisplayString() == "Turing.Interop.RustWrapped");

            var rustName = "##ERROR##";
            var x = attr?.ToString();
            if (x != null)
            {
                var start = x.IndexOf("\"", StringComparison.Ordinal)+1;
                rustName = x.Substring(start, x.LastIndexOf("\"", StringComparison.Ordinal)-start);
            }


            sb.AppendLine("private GCHandle internalMemoryHandle;");
            sb.AppendLine($"    public {className}Rs structInst;");
            sb.AppendLine($"    public {className}({wrapped.Type} wrappedObject)");
            sb.AppendLine("    {");
            sb.AppendLine($"        this.{wrapped.FieldName} = wrappedObject;");
            sb.AppendLine("        this.internalMemoryHandle = GCHandle.Alloc(this, GCHandleType.Pinned);");
            sb.AppendLine("        var instPtr = GCHandle.ToIntPtr(internalMemoryHandle);");
            sb.AppendLine($"        this.structInst = new {className}Rs {{ ptr = instPtr }};");
            sb.AppendLine("        Turing.Interop.WasmInterop.PersistentMemory.Add(this);");
            // sb.AppendLine($"        bind_{rustName}(structInst);");
            sb.AppendLine("    }");
            
            sb.AppendLine("    public void Dispose()");
            sb.AppendLine("    {");
            // sb.AppendLine($"        free_{rustName}(this.structInst);");
            sb.AppendLine("        internalMemoryHandle.Free();");
            sb.AppendLine("    }");
            
            methodDefs.Add(sb.ToString());
            
        }

        public void ProcessField(SourceProductionContext context, string className, FieldData field)
        {
            
        }
        
        string GetRsClassName(IMethodSymbol methodSymbol)
        {
            var containingType = methodSymbol.ContainingType;

            foreach (var member in containingType.GetMembers())
            {
                if (member is IFieldSymbol fieldSymbol)
                {
                    foreach (var attributeData in fieldSymbol.GetAttributes())
                    {
                        var x = "Turing.Interop.RustWrapped(\"".Length;
                        return attributeData.ToString().Substring(x, attributeData.ToString().Length-2 - x);
                        
                    }
                }
            }
            return null;
        }

        public void ProcessMethod(SourceProductionContext context, string className, MethodData method)
        {
            var name = method.Name;
            var sb = new StringBuilder();

            var ret = method.ReturnType;
            var retConverter = (opposite : "void", converterInfo: new MethodData());
            var retConverter2 = (opposite : "void", converterInfo: new MethodData());
            if (ret != "void")
            {
                retConverter = ConversionTypes[ret];
                retConverter2 = ConversionTypes[retConverter.opposite];
            }
            
            
            List<string> argConversions = [];
            
            List<string> parameters = [];
            List<string> args = [];

            var i = 1;
            foreach (var param in method.Parameters)
            {
                var paramType = param.Type;
                var paramTypeConverter = ConversionTypes[paramType];
                
                args.Add($"{param.Name}Converted");
                parameters.Add($"{paramTypeConverter.opposite} {param.Name}");
                argConversions.Add($"        var {param.Name}Converted = Codec.{paramTypeConverter.converterInfo.Name}(convertedParams.GetParameter<{paramTypeConverter.opposite}>({i++}));");
            }
            var joinedParams = string.Join(", ", parameters.ToArray());
            parameters.Insert(0, $"{className}Rs instanceRs");
            var joinedParams2 = string.Join(", ", parameters.ToArray());

            var joinedArgs = string.Join(", ", args.ToArray());

            sb.AppendLine("");
            sb.AppendLine($"    {PtrDeco}");

            sb.AppendLine($"    public delegate Turing.Interop.Parameters.RsParams Delegate{name}(Turing.Interop.Parameters.RsParams rsParams);");
            // sb.AppendLine($"    private delegate {retConverter.opposite} Delegate{name}({joinedParams});");

            var attr = method.MethodSymbol?.GetAttributes().First(a => a.AttributeClass?.ToDisplayString() == "Turing.Interop.RustMethod");

            var rustName = "##ERROR##";
            var x = attr?.ToString();
            if (x != null)
            {
                var start = x.IndexOf("\"", StringComparison.Ordinal);
                rustName = x.Substring(start, x.LastIndexOf("\"", StringComparison.Ordinal)-start+1);
            }
            
            var class_rustName = GetRsClassName(method.MethodSymbol);

            delegateNames.Add(
                (
                    $"Delegate{name}",
                    $"{name}Rs",
                    "\"" + class_rustName + "_" + rustName.Substring(1, rustName.Length-1)
                )
            );

            sb.AppendLine("");
            sb.AppendLine($"    public static Turing.Interop.Parameters.RsParams {name}Rs(Turing.Interop.Parameters.RsParams rsParams)");
            // sb.AppendLine($"    public static {retConverter.opposite} {name}Rs({joinedParams2})");
            sb.AppendLine("    {");
            sb.AppendLine("        var convertedParams = Turing.Interop.Parameters.Parameters.Unpack(rsParams);");
            sb.AppendLine($"        var instance = ({className}) GCHandle.FromIntPtr(convertedParams.GetParameter<{className}Rs>(0).ptr).Target;");

            foreach (var conversion in argConversions)
            {
                sb.AppendLine(conversion);
            }

            if (ret != "void")
            {
                sb.AppendLine($"        var rsResult = instance.{name}({joinedArgs});");
                sb.AppendLine($"        return new Turing.Interop.Parameters.Parameters().Push(Codec.{retConverter2.converterInfo.Name}(rsResult)).Pack();");
            }
            else
            {
                sb.AppendLine($"        instance.{name}({joinedArgs});");
                sb.AppendLine("        return new Turing.Interop.Parameters.RsParams{ param_count = 0, params_array = IntPtr.Zero };");
            }

            sb.AppendLine("    }");
            
            methodDefs.Add(sb.ToString());
        }
        
    }

    private static void ConstructSource(SourceProductionContext context, ITypeSymbol classSymbol, StringBuilder codeBuilder, string className,
        FieldData? wrapped, List<FieldData> fields, List<MethodData> methods)
    {
        
        var constructor = new RustClassConstructor(className);

        if (wrapped != null)
        {
            constructor.ProcessWrapped(context, className, wrapped);
        }
        
        foreach (var field in fields)
        {
            constructor.ProcessField(context, className, field);
        }

        foreach (var method in methods)
        {
            constructor.ProcessMethod(context, className, method);
        }
        
        
        constructor.build(context, classSymbol, codeBuilder);
        
    }

    private static readonly DiagnosticDescriptor NonStaticMethodErrorDescriptor = new DiagnosticDescriptor(
        id: "TURING001",
        title: "Invalid Method",
        messageFormat: "Converter method '{0}' must be static",
        category: "CodeGeneration",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);
    
    private static readonly DiagnosticDescriptor BadParamCountMethodErrorDescriptor = new DiagnosticDescriptor(
        id: "TURING002",
        title: "Invalid Method",
        messageFormat: "Converter method '{0}' must have only 1 parameter",
        category: "CodeGeneration",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    private static void BuildCodec(SourceProductionContext context, INamedTypeSymbol classSymbol, string namespaceName,
        string className, List<ClassDeclarationSyntax> RustClasses, Compilation compilation)
    {
        var converters = classSymbol.GetMembers()
            .OfType<IMethodSymbol>()
            .Where(m => m.GetAttributes()
                .Any(a => a.AttributeClass?.ToDisplayString() == "Turing.Interop.Converter"))
            .Select(m => new { m, data = new
            {
                m.Name,
                ReturnType = m.ReturnType.ToDisplayString(),
                m.IsStatic,
                Parameters = m.Parameters.Select(p => new
                {
                    p,
                    p.Name,
                    Type = p.Type.ToDisplayString(),
                }).ToList()
            }}).ToList();

        var mapBuilder = new StringBuilder();

        mapBuilder.AppendLine("// <auto-generated/>");
        mapBuilder.AppendLine($"namespace {namespaceName} {{");
        mapBuilder.AppendLine($"    public partial class {className} {{");
        mapBuilder.AppendLine("        private static readonly System.Collections.Generic.Dictionary<string, System.Func<object, object>> converters = new System.Collections.Generic.Dictionary<string, System.Func<object, object>>()");
        mapBuilder.AppendLine("        {");
        
        ConversionTypes.Clear();

        foreach (var methodData in converters)
        {

            var isInvalid = false;
            
            if (!methodData.data.IsStatic)
            {
                isInvalid = true;
                context.ReportDiagnostic(Diagnostic.Create(
                    NonStaticMethodErrorDescriptor,
                    methodData.m.Locations.First(),
                    methodData.m.Name));
            }

            if (methodData.data.Parameters.Count == 0)
            {
                isInvalid = true;
                context.ReportDiagnostic(Diagnostic.Create(
                    BadParamCountMethodErrorDescriptor,
                    methodData.m.Locations.First(),
                    methodData.m.Name));
            }
            else if (methodData.data.Parameters.Count != 1)
            {
                isInvalid = true;
                foreach (var param in methodData.data.Parameters.GetRange(1, methodData.data.Parameters.Count - 1))
                {
                    context.ReportDiagnostic(Diagnostic.Create(
                        BadParamCountMethodErrorDescriptor,
                        param.p.Locations.First(),
                        methodData.m.Name));
                }
            }
            
            if (isInvalid) continue;

            ConversionTypes.Add(methodData.data.ReturnType, (methodData.data.Parameters[0].Type, new MethodData
            {
                MethodSymbol = methodData.m,
                Name = methodData.data.Name,
                ReturnType = methodData.data.ReturnType,
                IsStatic = methodData.data.IsStatic,
                Parameters = methodData.data.Parameters.Select(p => new ParameterData
                {
                    Parameter = p.p,
                    Name = p.Name,
                    Type = p.Type
                }).ToList()
            }));
            
            mapBuilder.AppendLine($"            {{ \"{methodData.data.ReturnType}\", v => {methodData.data.Name}(({methodData.data.Parameters[0].Type}) v) }},");
            
        }

        var sb = new StringBuilder();
        
        foreach (var rustClass in RustClasses)
        {
            
            // We need to get semantic model of the class to retrieve metadata.
            var semanticModel = compilation.GetSemanticModel(rustClass.SyntaxTree);

            // Symbols allow us to get the compile-time information.
            if (semanticModel.GetDeclaredSymbol(rustClass) is not INamedTypeSymbol rsClassSymbol)
                continue;
            var csClass = rsClassSymbol.ToDisplayString();
            
            var rsClass = csClass + "Rs";
            
            var csClassFiltered = csClass.Replace(".", "_");
            var rsClassFiltered = csClassFiltered + "Rs";

            sb.AppendLine($"        public static {csClass} {rsClassFiltered}To{csClassFiltered}({rsClass} rs) {{");
            sb.AppendLine($"            return ({csClass}) GCHandle.FromIntPtr(rs.ptr).Target;");
            sb.AppendLine("        }");
            
            sb.AppendLine($"        public static {rsClass} {csClassFiltered}To{rsClassFiltered}({csClass} cs) {{");
            sb.AppendLine("            return cs.structInst;");
            sb.AppendLine("        }");

        }
        
        mapBuilder.AppendLine("        };");
        
        mapBuilder.AppendLine("        public static System.Func<object, object> getConverter(string type) {");
        mapBuilder.AppendLine("            return converters[type];");
        mapBuilder.AppendLine("        }");
        
        mapBuilder.AppendLine("    }");
        mapBuilder.AppendLine("}");
        
        context.AddSource($"{className}.g.cs", mapBuilder.ToString());
        
    }


    private void BuildBindCalls(SourceProductionContext context, INamedTypeSymbol classSymbol, string namespaceName,
        string className)
    {
        
        var attr = classSymbol.GetAttributes().FirstOrDefault(a => a.AttributeClass?.ToDisplayString() == "Turing.Interop.InteropClass");

        var rustName = "##ERROR##";
        var x = attr?.ToString();
        if (x != null)
        {
            var start = x.IndexOf("\"", StringComparison.Ordinal)+1;
            rustName = x.Substring(start, x.LastIndexOf("\"", StringComparison.Ordinal)-start);
        }

        var sb = new StringBuilder();
        sb.AppendLine("// <auto-generated/>");
        sb.AppendLine("using System;");
        sb.AppendLine("using System.Runtime.InteropServices;");
        sb.AppendLine("using Turing.Interop.Parameters;");
        sb.AppendLine($"namespace {namespaceName}");
        sb.AppendLine("{");
        sb.AppendLine($"    public static partial class {className}");
        sb.AppendLine("    {");
        
        var callbackMethods = classSymbol.GetMembers()
            .OfType<IMethodSymbol>()
            .Where(m => m.GetAttributes()
                .Any(a => a.AttributeClass?.ToDisplayString() == "Turing.Interop.RustCallback"));

        sb.AppendLine("        private static System.Collections.Generic.List<Delegate> _keepAlive = new System.Collections.Generic.List<Delegate>();");

        
        foreach (var callback in callbackMethods)
        {
            var callbackAttr = callback.GetAttributes().FirstOrDefault(a => a.AttributeClass?.ToDisplayString() == "Turing.Interop.RustCallback");

            var callbackRustName = "##ERROR##";
            var y = callbackAttr?.ToString();
            if (y != null)
            {
                var start = y.IndexOf("\"", StringComparison.Ordinal);
                callbackRustName = y.Substring(start, y.LastIndexOf("\"", StringComparison.Ordinal)-start+1);
            }

            sb.AppendLine("        [System.Runtime.InteropServices.UnmanagedFunctionPointer(System.Runtime.InteropServices.CallingConvention.Cdecl)]");
            sb.AppendLine($"        public delegate RsParams _{callback.Name}(RsParams parameters);");

            sb.AppendLine($"        public static RsParams Wrapped{callback.Name}(RsParams parameters)");
            sb.AppendLine("        {");
            sb.AppendLine("            var turing_unpacked = Turing.Interop.Parameters.Parameters.Unpack(parameters);");

            List<string> args = [];
            var i = 0;
            foreach (var param in callback.Parameters)
            {
                var paramType = param.Type.ToDisplayString();
                var paramTypeConverter = ConversionTypes[paramType];

                sb.AppendLine($"            var {param.Name} = Turing.Interop.Codec.{paramTypeConverter.converterInfo.Name}( turing_unpacked.GetParameter<{paramTypeConverter.opposite}>({i++}) );");
                args.Add(param.Name);
            }
            
            
            var ret = callback.ReturnType.ToDisplayString();
            var retConverter = (opposite : "void", converterInfo: new MethodData());
            var retConverter2 = (opposite : "void", converterInfo: new MethodData());
            if (ret != "void")
            {
                retConverter = ConversionTypes[ret];
                retConverter2 = ConversionTypes[retConverter.opposite];
                sb.AppendLine($"            var ret_out = Turing.Interop.Codec.{retConverter2.converterInfo.Name}({callback.Name}({string.Join(", ", args)}));");
                sb.AppendLine($"            return new Turing.Interop.Parameters.Parameters().Push(ret_out).Pack();");
            }
            else
            {
                sb.AppendLine($"            {callback.Name}({string.Join(", ", args)});");
                sb.AppendLine($"            return new Turing.Interop.Parameters.Parameters().Pack();");
            }
            
            sb.AppendLine("        }");

            sb.AppendLine($"        public static void Bind{callback.Name}()");
            sb.AppendLine("        {");
            sb.AppendLine($"            var del = Delegate.CreateDelegate(typeof(_{callback.Name}), typeof({classSymbol.ToDisplayString()}).GetMethod(\"Wrapped{callback.Name}\"));");
            sb.AppendLine("            _keepAlive.Add(del);");
            sb.AppendLine("            var funcPtr = System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate(del);");
            sb.AppendLine($"            var namePtr = System.Runtime.InteropServices.Marshal.StringToHGlobalAnsi({callbackRustName});");
            sb.AppendLine("            register_function(namePtr, funcPtr);");
            sb.AppendLine("            System.Runtime.InteropServices.Marshal.FreeHGlobal(namePtr);");
            sb.AppendLine("        }");

            interopBinders.Add($"Bind{callback.Name}();");

        }
        

        sb.AppendLine($"        public static void {rustName}()");
        sb.AppendLine($"        {{");
        
        foreach (var callback in interopBinders)
        {
            sb.AppendLine($"            {callback}");
        }
        
        sb.AppendLine("        }");
        sb.AppendLine("    }");
        sb.AppendLine("}");
        
        
        context.AddSource($"{className}Binding.g.cs", sb.ToString());
    }
    

}