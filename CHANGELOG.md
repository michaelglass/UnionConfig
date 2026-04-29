# Changelog

## Unreleased

## 0.2.0 - 2026-04-29

- feat: add `External` `ConfigVarKind` for runtime-only values (operator-injected tokens, sidecar secrets) that must never be persisted to `.env` or SSM
- feat: add `ConfigVarKind.isPersistable` helper for filtering vars before env-edit / SSM-write flows
- fix: disable SourceLink when no .git directory exists (jj without colocated git)
- chore: replace bespoke scripts with shared NuGet tools and reusable workflows (CoverageRatchet, SyncDocs, FsSemanticTagger, FsProjLint)
- chore: wire up auto-discovering example-projects in CI workflow
- chore: bump NuGet dependencies and shared tool versions

## v0.1.0

- feat: add `missingEntriesHeader` and `headerLines` support to `writeEnvFile`
- fix: cover all `ConfigVarKind` branches and add `--project` flag to test commands
- fix: use `List.isEmpty` instead of `= []` to satisfy FSharpLint

## v0.1.0-alpha.1

- feat: add `ConfigRegistry` for reflection-based DU config discovery — replaces manual `allCases` arrays with automatic DU case enumeration; supports flat DUs and one level of nested DUs for modular config ownership
- feat: add `writeEnvFile` and `defaultSections` to `EnvFile` — writes sectioned `.env` files with headers and comments; `defaultSections` builds sections from grouped defs using `Doc.Description` as entry comments
- feat: change `Reader.read` to return `Result` instead of throwing
- refactor: consolidate `UnionConfig`, `UnionConfig.Ssm`, and `UnionConfig.TextEditor` into a single `UnionConfig` package with zero external dependencies
- refactor: replace `SsmClient` with `SsmOperations` record for improved testability
- fix: fail explicitly for unsupported DU case shapes in `ConfigRegistry`
- fix: remove `Group` field from `ConfigVarDef`
- docs: rewrite README with full public API coverage, `WarnOn` docs, and gitignore settings
