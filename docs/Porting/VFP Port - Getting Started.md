# Porting applications from VFP to X\#

## General Information

VFP compatibility for X# is currently under active development. We are proud of already
reaching a very high level of compatibility for our customers coming from Visual Objects,
allowing already several dozens of VO applications to be ported to X# with minimum changes
required, and we are confident that we will manage to do the same for VFP! We have a dedicated
team member actively working on the VFP runtime to ensure the highest possible compatibility,
and we expect to make progress at a rapid pace.

The development effort is being carried out in the following phases:

1. **Built-in Functions:** Our immediate priority is to cover as many VFP built-in functions as
possible, aiming to reach a high level of drop-in compatibility for existing FoxPro code.

2. **Object-Oriented Programming (OOP):** Once the built-in function surface is solid, we will
shift focus to the OOP layer, divided into two sub-phases:
   - *OOP Core:* Base classes, inheritance, member modifiers, and core object model behavior.
   - *OOP GUI:* Controls, forms, and visual components.

3. **Commands Inventory:** A thorough inventory of all supported VFP commands is underway.
Where gaps are identified, they will be prioritized and implemented.

4. **Data Access:** We will evaluate and expand support for DBF tables, DBC databases, ODBC,
and other data access mechanisms native to VFP.

5. **Reports:** A VFP-compatible report engine is being developed, with the goal of supporting
both standalone and embedded report generation.

Once we reach an overall compatibility coverage of at least 85%, we will turn our full
attention to maturing the **VFPXPorter** tool (which already has a solid foundation thanks to
the initial work done by our colleague Fabrice) to provide a smooth automated migration
experience from VFP projects to X# solutions.

> **Important:** As with the VO to X# migration, 100% compatibility is not possible due to
> fundamental design differences between the VFP runtime and the .NET Framework. Manual
> adjustments will always be necessary, ranging from trivial syntactic changes to more
> involved code design adaptations. However, our goal is to minimize this effort as much as
> technically possible.

---

## Current State of Compatibility

### What is working already

- **Compiler:** The full Visual FoxPro language syntax is supported by the X# compiler when
using the VFP dialect. If you encounter any valid VFP code snippets that trigger compiler
errors, please let us know — your feedback is essential to close any remaining gaps.

- **DBF System:** We fully support the FoxPro RDD (Replaceable Data Driver), including
special data types (Currency, DateTime, Time, binary, Variant), auto-increment fields,
and NULL support.

- **Commands:** The majority of VFP commands are already implemented in X# and we are
actively filling any gaps at a rapid pace.

- **VFPXPorter (initial version):** A tool that converts your VFP project files (code,
forms, visual class libraries, etc.) into an X# solution. This tool already handles a
substantial portion of the conversion work, though it will be revisited and matured
further as the runtime reaches higher compatibility.

### In Progress

- **Built-in Functions:** Our developer is actively working through the VFP built-in
function catalog, implementing missing functions and ensuring behavior matches the
original VFP runtime as closely as possible.

- **Report System:** We are currently working on implementing a fully VFP-compatible report
system, capable of rendering existing VFP reports (FRX files). We expect to have it
finished by the end of summer 2026. A standalone designer and an embedded designer
(for use within your applications) are also planned.

### Planned for the near future

- **OOP Core Layer:** Base classes, proper inheritance, member visibility, and the full
object model that VFP developers rely on.

- **OOP GUI Layer:** Controls, form classes, containers, and the visual component model.

- **Command Inventory & Gap Analysis:** A systematic review of all VFP commands to
identify and prioritize those still missing.

- **Data Access:** Extended support for DBC databases, SQL passthrough, ODBC connectivity,
and other data access patterns used in VFP applications.

- **VFPXPorter Maturation:** Retake and enhance the exporter tool to align it with the
matured runtime, ensuring it can handle real-world VFP project structures.

---

## Migration Paths

Not every VFP developer is in the same situation. Depending on your background and the
size and complexity of your existing VFP codebase, different approaches may be appropriate.
Below we outline the main scenarios and our recommended paths for each.

### Scenario 1: You are a FoxPro developer with NO large legacy applications

If you are coming from FoxPro out of curiosity, to learn, or to explore what X# can offer,
the entry point is straightforward:

- **Choose your IDE:** You can use **XIDE** (the lightweight X# IDE with a familiar feel
for xBase developers) or **Visual Studio** (the industry-standard .NET IDE). Both have
full support for the VFP dialect in X#.

- **Start small:** Create a new console application, experiment with FoxPro commands and
functions, and get comfortable with the build-debug cycle.

- **Explore WinForms:** Once you are comfortable, create small Windows Forms applications
to see how the FoxPro dialect interacts with the .NET UI framework.

- **Learn .NET at your own pace:** There is no rush. Familiarize yourself with the .NET
Base Class Library, collections, LINQ, and other modern features at your own speed.

### Scenario 2: You have large legacy VFP applications but cannot migrate everything at once

This is the most common case. You have a significant investment in VFP code that works,
and a full rewrite or migration is not feasible in the short term. We recommend a
**bottom-up, incremental approach**:

1. **Set up your environment with the VFP dialect:** Create X# projects configured to
use the FoxPro dialect. This allows you to write and compile code using familiar VFP
commands, classes, and functions, running on top of the .NET runtime.

2. **Build confidence with small solutions:** Start by creating small, self-contained
parts of your system in X# — console utilities, helper tools, or isolated WinForms
windows. This will give you a feel for the workflow without risking your main
application.

3. **Get comfortable with the syntax and .NET:** As you work on these smaller pieces,
you will naturally pick up .NET concepts — namespaces, assemblies, the class library,
and modern debugging. The goal is not to abandon FoxPro syntax, but to blend it
effectively with the .NET ecosystem.

4. **Build .NET bridge libraries for your legacy VFP app:** This is a powerful intermediate
step. Compile X# class libraries targeting .NET Framework 4.8 or .NET 6+ and consume
them from your existing VFP application using bridge tools such as wwDotNetBridge or FoxBridge.
   
This allows you to start leveraging modern .NET capabilities (cryptography, HTTP
clients, JSON/XML processing, cloud APIs, etc.) directly from your current VFP
application, without migrating a single line of legacy code.

5. **(Future) Migrate fully with VFPXPorter:** Once the runtime is mature and the
VFPXPorter tool has been refined, you will be in an excellent position to port your
entire application in one pass, already having a solid understanding of the X#
environment.

### Scenario 3: Full migration via VFPXPorter (Future path)

For those aiming to eventually port their entire VFP application to X# in one go, the
VFPXPorter tool will be the recommended migration vehicle. This tool performs an automated
conversion of your VFP project (PJX/PJ2 files, forms, class libraries, menus, reports,
and code) into a ready-to-compile X# solution.

> **Current status:** The VFPXPorter already has a strong foundation, but it will be
> revisited and brought up to date once the runtime reaches at least 85% compatibility.
> At that point, we will ensure the exporter handles real-world project structures,
> 3rd party controls, and edge cases that are common in legacy FoxPro applications.

---

## Documentation & Resources

Before diving into porting your VFP applications, make sure you are familiar with the
available resources:

- **X# Documentation:** The official documentation covers the VFP dialect, supported
commands and functions, and general X# concepts:
  <https://www.xsharp.eu/help>

- **X# Forum:** Our community forum is the best place to ask questions, share experiences,
and get help from both the development team and fellow X# developers. If you need
priority support from the X# development team, consider subscribing to the
[Friends Of XSharp (FOX)](https://www.xsharp.eu/store/shop/FOX-Subscription-c82913713) program.
  <https://www.xsharp.eu/forum>

- **Source Code:** The X# source code (compiler, runtime, and tools) is available for
reference and community contributions at:
  <https://github.com/X-Sharp>

---

## Future Plans

Beyond the phased roadmap described above, we are also exploring innovative approaches
to smooth the migration journey:

### In-Process VFP Engine Hosting

We are investigating the possibility of **hosting the native Visual FoxPro engine inside
an X# process**. This would serve as a fallback mechanism: when X# encounters a language
feature, function, or operation that is not yet implemented (or behaves differently from
VFP), the call could be delegated directly to the real VFP runtime engine.

This approach has several potential benefits:

- **Error fallback during migration:** If your ported application hits an unimplemented
feature, instead of crashing, it could transparently delegate to the VFP engine,
allowing you to continue development while we fill the gap.

- **Native report generation:** While the X# report engine is under development, the
embedded VFP engine could render 100% native VFP reports (FRX files), ensuring zero
visual discrepancies in printed output.

- **Gradual migration:** You could port your application incrementally, relying on the
fallback engine for the pieces that are not yet ready, and gradually replacing those
parts with pure X# code as the runtime matures.

> This feature is still in the research phase and is subject to technical feasibility
> and licensing considerations.

---

## We can port your application for you

If you prefer not to spend the time porting your VFP application(s) on your own, we can
do it for you. We have successfully ported several applications already (from Visual
Objects) and we are ready to offer the same service for FoxPro applications. At a
determined fee (depending on the size and complexity of your system), you can send us
your source code and we will send you back a ported version working in X#.

If you are interested in this service, please contact us at **info@xsharp.eu**

---

## After porting your app

You have ported your VFP application to X# — well done! This has already given many more
years of life to it, as it is now developed in a modern, very widely used and supported
environment. It should also be more stable and robust than it used to be, thanks to the
.NET Framework's far superior Memory Management and Garbage Collector.

From now on, you can continue its development purely in X#, and make use of the unlimited
amount of features and tools that .NET has to offer:

- 64-bit numeric variables
- The `Decimal` data type for precise currency calculations
- Generics and strongly-typed collections
- Modern language constructs: interfaces, events, enumerations, LINQ, async/await
- Easy multithreading and parallel processing
- Unicode strings out of the box
- A vast ecosystem of classes in the .NET Framework itself
- All third-party libraries written in any .NET language (C#, VB.NET, F#) are
  automatically available to X# as well
- Modern web development with ASP.NET Core
- Cross-platform support via .NET 6+

The list of new features is endless, and they are waiting for you to explore and
incorporate into your application development from now on!
