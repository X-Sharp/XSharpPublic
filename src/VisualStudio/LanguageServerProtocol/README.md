# XSharp Language Server Protocol component

This project contains a Visual-Studio-independent language service core for XSharp based on the `XSharp.CodeModel` project.

## Goals

- Parse source code using CodeModel (`XFile.ParseContents`, `SourceWalker`, `XProject`)
- Resolve types, members, and symbols through CodeModel lookup APIs
- Expose reusable LSP-oriented operations without any Visual Studio API dependency

## Implemented feature surface

- document open/change/close workspace synchronization
- hover
- completion
- go to definition
- find references
- document symbols
- workspace symbols
- signature help

## Notes

- This project is transport-agnostic: it exposes reusable service methods and LSP-shaped DTOs.
- IDE-specific hosts (VS Code, Neovim, JetBrains, etc.) can map JSON-RPC requests to `XSharpLspLanguageService` calls.
