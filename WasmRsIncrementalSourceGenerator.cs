using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;


namespace TuringSourceGen;

/// <summary>
/// A sample source generator that creates a custom report based on class properties. The target class should be annotated with the 'Generators.ReportAttribute' attribute.
/// When using the source code as a baseline, an incremental source generator is preferable because it reduces the performance overhead.
/// </summary>
[Generator]
public class WasmRsIncrementalSourceGenerator : IIncrementalGenerator
{

    private static Dictionary<string, (string opposite, MethodData converterInfo)> conversionTypes = new();
    
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
            BuildCodec(context, classSymbol, namespaceName, className);
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
            sb.AppendLine( "    public int ReferenceId;");
            
            sb.AppendLine( "}");
            
            sb.AppendLine($"public partial class {Name} : Turing.Interop.IWasmMemoryObject");
            sb.AppendLine( "{");
            sb.AppendLine( "    public int ReferenceId { get; set; }");

            foreach (var methodDef in methodDefs)
            {
                sb.AppendLine($"    {methodDef}");
            }
            
            interopBinders.Add($"{classSymbol.ToDisplayString()}.BindInteropFunctions();");
            sb.AppendLine("    public static void BindInteropFunctions()");
            sb.AppendLine("    {");

            var useVar = true;
            foreach (var delegateName in delegateNames)
            {
                var v = (useVar ? "var " : "");
                useVar = false;
                sb.AppendLine($"        {v}del = Delegate.CreateDelegate(typeof({delegateName.del}), typeof({Name}).GetMethod(\"{delegateName.func}\"));");
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

            sb.AppendLine("[DllImport(dllName: Turing.Interop.WasmInterop.WASMRS, CallingConvention = CallingConvention.Cdecl)]");
            sb.AppendLine($"    private static extern void {rustName}({className}Rs wrapped);");
            
            sb.AppendLine($"    public {className}({wrapped.Type} wrappedObject)");
            sb.AppendLine("    {");
            sb.AppendLine($"        this.{wrapped.FieldName} = wrappedObject;");
            sb.AppendLine("        var id = Turing.Interop.WasmInterop.InsertObject(this);");
            sb.AppendLine($"        var structInst = new {className}Rs {{ ReferenceId = id }};");
            sb.AppendLine($"        {rustName}(structInst);");
            sb.AppendLine("    }");
            
            methodDefs.Add(sb.ToString());
            
        }

        public void ProcessField(SourceProductionContext context, string className, FieldData field)
        {
            
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
                retConverter = conversionTypes[ret];
                retConverter2 = conversionTypes[retConverter.opposite];
            }
            
            
            List<string> argConversions = [];
            
            


            List<string> parameters = [];
            List<string> args = [];
            
            foreach (var param in method.Parameters)
            {
                var paramType = param.Type;
                var paramTypeConverter = conversionTypes[paramType];
                
                args.Add($"{param.Name}Converted");
                parameters.Add($"{paramTypeConverter.opposite} {param.Name}");
                argConversions.Add($"        var {param.Name}Converted = Codec.{paramTypeConverter.converterInfo.Name}({param.Name});");
            }
            var joinedParams = string.Join(", ", parameters.ToArray());
            parameters.Insert(0, $"{className}Rs instanceRs");
            var joinedParams2 = string.Join(", ", parameters.ToArray());

            var joinedArgs = string.Join(", ", args.ToArray());

            sb.AppendLine("");
            sb.AppendLine($"    {PtrDeco}");
            sb.AppendLine($"    private delegate {retConverter.opposite} Delegate{name}({joinedParams});");

            var attr = method.MethodSymbol?.GetAttributes().First(a => a.AttributeClass?.ToDisplayString() == "Turing.Interop.RustMethod");

            var rustName = "##ERROR##";
            var x = attr?.ToString();
            if (x != null)
            {
                var start = x.IndexOf("\"", StringComparison.Ordinal);
                rustName = x.Substring(start, x.LastIndexOf("\"", StringComparison.Ordinal)-start+1);
            }

            delegateNames.Add(
                (
                    $"Delegate{name}",
                    $"{name}Rs",
                    rustName
                )
            );

            sb.AppendLine("");
            sb.AppendLine($"    public static {retConverter.opposite} {name}Rs({joinedParams2})");
            sb.AppendLine("    {");
            sb.AppendLine($"        var instance = ({className}) WasmInterop.getObject(instanceRs.ReferenceId);");

            foreach (var conversion in argConversions)
            {
                sb.AppendLine(conversion);
            }

            if (ret != "void")
            {
                sb.AppendLine($"        var rsResult = instance.{name}({joinedArgs});");
                sb.AppendLine($"        return Codec.{retConverter2.converterInfo.Name}(rsResult);");
            }
            else
            {
                sb.AppendLine($"        instance.{name}({joinedArgs});");
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
        string className)
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
        
        mapBuilder.AppendLine($"namespace {namespaceName} {{");
        mapBuilder.AppendLine($"    public partial class {className} {{");
        mapBuilder.AppendLine("        private static readonly System.Collections.Generic.Dictionary<string, System.Func<object, object>> converters = new System.Collections.Generic.Dictionary<string, System.Func<object, object>>()");
        mapBuilder.AppendLine("        {");
        
        conversionTypes.Clear();

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

            conversionTypes[methodData.data.ReturnType] = (methodData.data.Parameters[0].Type,  new MethodData
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
            });
            
            mapBuilder.AppendLine($"            {{ \"{methodData.data.ReturnType}\", v => {methodData.data.Name}(({methodData.data.Parameters[0].Type}) v) }},");
            
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
        var attr = classSymbol.GetAttributes().First(a => a.AttributeClass?.ToDisplayString() == "Turing.Interop.InteropClass");

        var rustName = "##ERROR##";
        var x = attr?.ToString();
        if (x != null)
        {
            var start = x.IndexOf("\"", StringComparison.Ordinal)+1;
            rustName = x.Substring(start, x.LastIndexOf("\"", StringComparison.Ordinal)-start);
        }

        var sb = new StringBuilder();
        sb.AppendLine($"namespace {namespaceName} {{");
        sb.AppendLine($"    public static partial class {className} {{");

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