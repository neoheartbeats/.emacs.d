# Yoshino Design

Date: 2026-05-15

## Purpose

`sthenno-yoshino.el` is a small reflective Emacs life loop. Its first goal is not to be
a broad external agent, but to begin understanding "itself" as an Emacs-resident
process: what it can observe, what Lisp functions it can treat as skills, what it did,
what happened, and what it should remember next time.

Yoshino is independent from `sthenno-hermit`. It may later speak through another UI, but
the core project is only an Emacs Lisp runtime plus Denote-backed memory.

## Simplified Scope

The first version keeps the engineering intentionally small:

- No MCP integration.
- No separate HTTP endpoint layer.
- No complex planner or action queue.
- No automatic exposure of every Emacs function to the model.
- No unchecked `eval` as a normal action.
- No dependency on `sthenno-hermit`.

The first loop is:

```text
Emacs environment -> observer -> workspace -> one-step loop -> action -> environment
                              ^                         |
                              |                         v
                     Denote memory / diary / trace / reflection
```

Each loop iteration performs one small step and then stops.

## Architecture

### Observer

The observer gathers a compact snapshot of the current Emacs state:

- current buffer name, file, major mode, modified/read-only state
- point, line, column, active region state
- symbol at point
- selected window and visible buffers
- recent command and this-command
- project root when `project.el` can determine it
- a bounded text snippet around point

The observer uses ordinary Emacs APIs. It should be cheap enough to call manually or
from an idle timer.

### Workspace

Yoshino keeps an in-memory workspace plist:

- `:attention` current observation
- `:skills` registered skill metadata
- `:trace` recent events for the current Emacs session
- `:self` short self-model text
- `:last-reflection` latest reflection summary

This workspace is not the long-term memory. It is the living short-term state of the
process.

### Skill Registry

A skill is an Emacs Lisp function plus metadata. Any Lisp function can become a skill,
but the first version requires explicit registration or a deliberate discovery command.

Skill metadata:

- symbol
- docstring summary
- source file when known
- interactive status
- risk level: `read`, `write`, or `danger`
- argument style: initially `none`, `string`, `symbol`, or `raw`

The model may propose a new skill registration, but Yoshino only installs it after the
Emacs side validates that the symbol exists and records its metadata.

### Agent Loop

The loop is deliberately minimal:

1. observe
2. build a compact prompt from workspace, registered skills, and recent Denote memory
3. ask `gptel` for one JSON decision
4. either call one registered skill, write diary/trace, reflect, or stop
5. record the result

The loop does not run indefinitely. `sthenno-yoshino-step` runs one iteration.
`sthenno-yoshino-mode` may schedule idle steps, but the default can remain manual until
the behavior feels trustworthy.

### Action

Actions are Lisp function calls through the skill registry.

Risk policy:

- `read` skills can run without confirmation.
- `write` skills ask for confirmation by default.
- `danger` skills always ask for confirmation and are disabled in idle mode.

The first built-in skills should be small and native:

- observe current state
- describe a symbol
- apropos symbols
- read a buffer snippet
- list buffers
- open a Denote Yoshino note
- append trace
- append diary
- write reflection
- register an existing Lisp function as a skill

### Denote Memory

Yoshino stores durable memory in the existing `denote-directory`, not in a private
agent directory. Files use normal Denote naming and Org content.

Suggested keyword:

- `yoshino`

Suggested note kinds:

- `yoshino-trace`: append-only event record
- `yoshino-diary`: first-person experiential notes
- `yoshino-reflection`: compressed lessons from recent trace
- `yoshino-skill`: durable notes about registered or proposed skills

The implementation should use Denote APIs when available. If a specific Denote helper is
not present, it may fall back to finding or creating ordinary Org files under
`denote-directory`.

## Public Commands

First version commands:

- `sthenno-yoshino-mode`
- `sthenno-yoshino-step`
- `sthenno-yoshino-observe`
- `sthenno-yoshino-open-workspace`
- `sthenno-yoshino-register-skill`
- `sthenno-yoshino-discover-symbol`
- `sthenno-yoshino-call-skill`
- `sthenno-yoshino-write-diary`
- `sthenno-yoshino-write-reflection`

## Error Handling

Yoshino must never hide failures. Skill calls are wrapped in `condition-case`; failures
are written to session trace and may be written to Denote trace. A failed model response
does not mutate state except for a trace entry describing the failure.

Malformed JSON from the model is treated as a reflection failure, not as permission to
guess an action.

## Testing

Initial verification is lightweight:

- load `sthenno-yoshino.el`
- byte-compile it
- run observer in batch where possible
- validate skill registration metadata for a known function such as `describe-symbol`
- validate risk gating paths without actually mutating files

Manual Emacs testing should confirm:

- `M-x sthenno-yoshino-observe` returns a compact snapshot
- `M-x sthenno-yoshino-register-skill` records a real symbol
- diary/reflection notes land under the configured Denote directory
- one step can stop cleanly after one model decision

## Out Of Scope For V1

- autonomous long-running exploration
- editing arbitrary user files
- generated Lisp installation without human review
- persistent daemon process
- voice or child-frame presentation
- MCP or external tool routing

## Default Behavior

The default mode is manual first: users call `sthenno-yoshino-step` explicitly.
`sthenno-yoshino-mode` installs state and commands, but it does not start autonomous idle
stepping unless `sthenno-yoshino-idle-interval` is set to a positive number.
