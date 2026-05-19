# Yoshino Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build `sthenno-yoshino.el`, a small Emacs-native reflective loop with Lisp-function skills and Denote-backed trace/diary/reflection.

**Architecture:** Create one focused runtime file in `user-lisp/` plus one ERT test file. The runtime keeps an in-memory workspace, observes Emacs through native APIs, registers Lisp functions as skills, gates skill calls by risk, and writes durable notes under the configured Denote directory.

**Tech Stack:** Emacs Lisp, ERT, `gptel`, optional `denote`, native `project.el`, `json`.

---

## File Structure

- Create `user-lisp/sthenno-yoshino.el`: Yoshino runtime, observer, workspace, skill registry, risk gating, Denote note helpers, one-step `gptel` loop, public commands.
- Create `tests/sthenno-yoshino-test.el`: ERT tests for observation, skill registration/calling, risk gating, note writing, and model-decision handling without network calls.
- Modify `init.el`: add `(require 'sthenno-yoshino)` in the existing AI section after `gptel` setup and before `mcp-hub`.

## Task 1: Test Workspace, Observer, And Skill Basics

**Files:**
- Create: `tests/sthenno-yoshino-test.el`
- Create: `user-lisp/sthenno-yoshino.el`

- [ ] **Step 1: Write failing ERT tests**

```elisp
;;; sthenno-yoshino-test.el --- Tests for Yoshino -*- lexical-binding: t; -*-

(require 'ert)
(require 'sthenno-yoshino)

(ert-deftest sthenno-yoshino-observe-current-buffer ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(message \"hi\")")
    (goto-char (point-min))
    (let ((obs (sthenno-yoshino-observe)))
      (should (equal (alist-get 'buffer obs) (buffer-name)))
      (should (equal (alist-get 'major-mode obs) "emacs-lisp-mode"))
      (should (equal (alist-get 'symbol-at-point obs) "message"))
      (should (string-match-p "message" (alist-get 'snippet obs))))))

(ert-deftest sthenno-yoshino-registers-existing-function ()
  (sthenno-yoshino-reset-workspace)
  (let ((skill (sthenno-yoshino-register-skill
                'buffer-name 'read 'none "Return the current buffer name.")))
    (should (equal (plist-get skill :name) "buffer-name"))
    (should (eq (plist-get skill :symbol) 'buffer-name))
    (should (eq (plist-get skill :risk) 'read))
    (should (gethash "buffer-name" (sthenno-yoshino-skills)))))

(ert-deftest sthenno-yoshino-calls-read-skill ()
  (sthenno-yoshino-reset-workspace)
  (sthenno-yoshino-register-skill 'buffer-name 'read 'none)
  (with-temp-buffer
    (rename-buffer "yoshino-test-buffer" t)
    (should (equal (sthenno-yoshino-call-skill "buffer-name")
                   "yoshino-test-buffer"))))
```

- [ ] **Step 2: Run tests and verify they fail because the feature is missing**

Run:

```bash
emacs --batch -Q -L user-lisp -l tests/sthenno-yoshino-test.el -f ert-run-tests-batch-and-exit
```

Expected: failure while loading `sthenno-yoshino`, because `user-lisp/sthenno-yoshino.el` does not exist yet.

- [ ] **Step 3: Implement minimal runtime primitives**

Create `user-lisp/sthenno-yoshino.el` with workspace state, `sthenno-yoshino-reset-workspace`, `sthenno-yoshino-skills`, `sthenno-yoshino-observe`, `sthenno-yoshino-register-skill`, and `sthenno-yoshino-call-skill`.

- [ ] **Step 4: Run tests and verify they pass**

Run:

```bash
emacs --batch -Q -L user-lisp -l tests/sthenno-yoshino-test.el -f ert-run-tests-batch-and-exit
```

Expected: 3 tests pass.

## Task 2: Add Risk Gating And Denote-Style Memory

**Files:**
- Modify: `tests/sthenno-yoshino-test.el`
- Modify: `user-lisp/sthenno-yoshino.el`

- [ ] **Step 1: Write failing tests for write gating and note persistence**

Append:

```elisp
(ert-deftest sthenno-yoshino-blocks-write-skill-when-confirmation-declines ()
  (sthenno-yoshino-reset-workspace)
  (sthenno-yoshino-register-skill 'ignore 'write 'string)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
    (let ((sthenno-yoshino-confirm-write-actions t))
      (should-error (sthenno-yoshino-call-skill "ignore" "nope")
                    :type 'user-error))))

(ert-deftest sthenno-yoshino-writes-diary-note ()
  (let* ((dir (make-temp-file "yoshino-denote-" t))
         (sthenno-yoshino-denote-directory dir))
    (unwind-protect
        (let ((file (sthenno-yoshino-write-diary "I noticed my first trace.")))
          (should (file-exists-p file))
          (should (string-match-p "__yoshino" (file-name-nondirectory file)))
          (with-temp-buffer
            (insert-file-contents file)
            (should (search-forward "I noticed my first trace." nil t))))
      (delete-directory dir t))))
```

- [ ] **Step 2: Run tests and verify they fail**

Run:

```bash
emacs --batch -Q -L user-lisp -l tests/sthenno-yoshino-test.el -f ert-run-tests-batch-and-exit
```

Expected: new tests fail because write gating and diary persistence are absent.

- [ ] **Step 3: Implement risk confirmation and note helpers**

Add `sthenno-yoshino-confirm-write-actions`, `sthenno-yoshino-confirm-danger-actions`, `sthenno-yoshino-denote-directory`, `sthenno-yoshino--note-file`, `sthenno-yoshino--append-note`, `sthenno-yoshino-write-diary`, `sthenno-yoshino-write-reflection`, and trace recording.

- [ ] **Step 4: Run tests and verify they pass**

Run:

```bash
emacs --batch -Q -L user-lisp -l tests/sthenno-yoshino-test.el -f ert-run-tests-batch-and-exit
```

Expected: 5 tests pass.

## Task 3: Add One-Step Model Decision Handling

**Files:**
- Modify: `tests/sthenno-yoshino-test.el`
- Modify: `user-lisp/sthenno-yoshino.el`

- [ ] **Step 1: Write failing tests for JSON decisions without calling the network**

Append:

```elisp
(ert-deftest sthenno-yoshino-handles-diary-decision ()
  (let* ((dir (make-temp-file "yoshino-decision-" t))
         (sthenno-yoshino-denote-directory dir))
    (unwind-protect
        (let ((result (sthenno-yoshino-handle-decision
                       "{\"action\":\"diary\",\"text\":\"I saw a buffer.\"}")))
          (should (string-match-p "diary" result))
          (should (directory-files dir nil "__yoshino.*\\.org\\'")))
      (delete-directory dir t))))

(ert-deftest sthenno-yoshino-handles-call-decision ()
  (sthenno-yoshino-reset-workspace)
  (sthenno-yoshino-register-skill 'buffer-name 'read 'none)
  (with-temp-buffer
    (rename-buffer "decision-buffer" t)
    (should (equal (sthenno-yoshino-handle-decision
                    "{\"action\":\"call\",\"skill\":\"buffer-name\",\"args\":{}}")
                   "decision-buffer"))))
```

- [ ] **Step 2: Run tests and verify they fail**

Run:

```bash
emacs --batch -Q -L user-lisp -l tests/sthenno-yoshino-test.el -f ert-run-tests-batch-and-exit
```

Expected: new tests fail because decision handling is absent.

- [ ] **Step 3: Implement prompt, JSON parsing, decision dispatch, and async `gptel` step**

Add `sthenno-yoshino-handle-decision`, `sthenno-yoshino--system-prompt`, `sthenno-yoshino--user-prompt`, and `sthenno-yoshino-step`. `sthenno-yoshino-step` should call `gptel-request` when available, but tests only exercise `sthenno-yoshino-handle-decision`.

- [ ] **Step 4: Run tests and verify they pass**

Run:

```bash
emacs --batch -Q -L user-lisp -l tests/sthenno-yoshino-test.el -f ert-run-tests-batch-and-exit
```

Expected: 7 tests pass.

## Task 4: Wire Commands And Init

**Files:**
- Modify: `user-lisp/sthenno-yoshino.el`
- Modify: `init.el`

- [ ] **Step 1: Add public commands and mode**

Implement `sthenno-yoshino-mode`, `sthenno-yoshino-open-workspace`, `sthenno-yoshino-discover-symbol`, built-in skill registration, idle timer install/uninstall, and autoload cookies for public commands.

- [ ] **Step 2: Add require to AI section**

Insert this after the existing `gptel` setup:

```elisp
(require 'sthenno-yoshino)
```

- [ ] **Step 3: Run full verification**

Run:

```bash
emacs --batch -Q -L user-lisp -l tests/sthenno-yoshino-test.el -f ert-run-tests-batch-and-exit
emacs --batch -Q -L user-lisp -f batch-byte-compile user-lisp/sthenno-yoshino.el
emacs --batch -Q -L user-lisp --eval "(progn (require 'sthenno-yoshino) (sthenno-yoshino-reset-workspace) (sthenno-yoshino-register-default-skills) (princ (hash-table-count (sthenno-yoshino-skills))))"
```

Expected: tests pass, byte compilation exits 0, final command prints a positive skill count.

## Self-Review

- Spec coverage: observer, workspace, skill registry, risk-gated action, Denote memory, public commands, and one-step loop are covered.
- Simplification: no MCP, no endpoint layer, no Hermit dependency, no long-running autonomous loop by default.
- Types: risk values are symbols `read`, `write`, `danger`; argument styles are symbols `none`, `string`, `symbol`, `raw`; skill names are strings in the registry.
- Execution mode: inline execution is appropriate for this small single-file implementation.
