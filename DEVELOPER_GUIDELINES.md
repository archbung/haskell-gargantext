# Developer guidelines

- [Developer guidelines](#developer-guidelines)
  - [Introduction](#introduction)
  - [Glossary](#glossary)
  - [Guidelines](#guidelines)
    - [Keep the issue tracker clean.](#keep-the-issue-tracker-clean)
    - [Keep CI green.](#keep-ci-green)
    - [Good issues, good merge requests.](#good-issues-good-merge-requests)
      - [Issues/feature requests](#issuesfeature-requests)
      - [Merge requests](#merge-requests)
    - [Don't push directly to `dev`, use feature branches.](#dont-push-directly-to-dev-use-feature-branches)
    - [Review all the things.](#review-all-the-things)
    - [Do not merge `WIP` merge requests.](#do-not-merge-wip-merge-requests)
    - [Write test.](#write-test)
    - [Features development lifecycle.](#features-development-lifecycle)
  - [Conclusion](#conclusion)
  - [Revisions](#revisions)

The aim of this document is to describe and document all the development best practices which should be put
in place when developing Gargantext. 

## Introduction

Gargantext is a dynamic project, which evolved from a Python project into a full-stack functional application,
with Haskell in the backend and PureScript in the frontend. As such, the project has seen many developers
come and go, with work sometimes contributed spontaneously by external people (being an open source,
free software). As every open source project, it's necessary to reach a common ground between developers and
share a set of development guidelines to make sure the project is developed _consistently_; by "consistently"
here we mean that there should be some common driving principles which should ultimately result in the
project being developed in a uniform way. In other words, there should be a clear process shared between
all developers about how to:

- How to code in a collaborative way;
- Propose and implement new features;
- Report problems and write good bug reports;
- Write tests (when, how, why);
- Manage work effectively between branches.

The rest of the document try to answer all those questions.

## Glossary

- GIT: _Git_ is a distributed version control system

- MR: _Merge Request_; usually called "Pull Request" in the GitHub work, is an
      event that takes place in software development when a contributor/developer
      is ready to begin the process of merging new code changes with the main
      project repository.

- CI: _Continuous Integration_; it's a term referring to the process of automatically
      building and testing our software on a remote machine. In Gargantext it's integrated
      in Gitlab via their _runners_. A "green CI" means that all the CI steps passes correctly,
      and typically these steps involve building and testing the process.

## Guidelines

The following is a non-exhaustive list of the development guidelines.

### Main working Branches

3 main branches are used in the distributed version control system (Git) of GarganText:
- _dev_ branch for latest development
- _testing_ branch for testing by beta tester 
- _stable_ branch for production only

Accordingly _dev_ commits are then merged into _testing_ then into
_stable_ if and only if the steps of this guideline are respected.

For development, please add the number of the issue in the branch you
are working on. For instance if you work on the issue with number #507,
use the following branch: dev-507-some-keywords-here

### Keep the issue tracker clean.

**Guideline: periodically scan the issue tracker for issues which are no longer relevant or are now
fixed, and close those tickets. If in doubt, ping the original author and ask for information.**

Periodically, we should try to make an effort in pruning old tickets, as well as closing issues
which are not relevant anymore. Let's remember that an issue tracker is not an append-only log, we are
supposed to make it grow _smaller_, not bigger: our primary goal should be to have the issue tracker be
as empty as possible. This seems obvious but has several advantages:

- It improves the external perception of the project: a project with hundreds of tickets gives the
  impression it's either abandoned or severely broken (i.e. "why has this project so many bugs?");
- It reduces the chances of _duplication_: the more tickets, the more confusion (for newcomers as well),
  and it's therefore easier to accidentally open a duplicate ticket (i.e. a ticket referring to an
  already-known bug or feature request).

### Keep CI green.

** Before asking for a Merge Request, make sure your branch has a green CI.**

**Guideline: We will not merge any MR which doesn't have green CI. 'docs' failures are allowed.**

Continuous Integration (e.g. CI) is a "public proof" that the project is building and
behaving correctly. Therefore, it's extremely important that we don't merge anything that doesn't
successfully pass CI. If this is slowing down development it's not a CI problem, it simply means
the project is not being kept to sufficiently high standards and we need to take a step back and
fix CI in order to be able to fearlessly march forward. Sometimes there are steps in the CI pipeline
which are marked as "optional" (like building the `docs`), and for those we tolerate failures.
Therefore, an MR in "Passed with warnings" status is acceptable, a MR with a "Failed" CI status is not.

### Good issues, good merge requests.

**Guideline: Write issues and merge requests with a clear description that states the intention and/or
the rationale. For issues, try to add reproduction steps.**

#### Issues/feature requests

When opening a new issue, make sure to do the following:

- If this is a feature request, describe why this would be a useful addition and which problem it would solve;
- If this is a bug report, try to clearly spell out:
    * What the bug exactly is;
    * How does the bug manifests itself;
    * If relevant, useful system information like the version of GHC or the operating system.
    * If possible, include some reproduction steps (e.g. "click on button X, select Y, etc etc").

#### Merge requests

When opening a MR, make sure to do the following:

- Check your branch has a green CI first please. If you need help or
  discussion, use the comments in the issue related to your current
  working branch.
- If this is closing an issue, consider using one of the [closing patterns](https://docs.gitlab.com/ee/user/project/issues/managing_issues.html#default-closing-pattern)
  to automatically close the associated issue when the MR is merged. For example, you can say "Fixes #XXX"
  (where `XXX` is an issue number) in the MR description;
- Add a good description in the MR which summarises the problem which is solving and, if possible, the
  approach taken to solve the issue: this will help reviewers (see "Review all the things").

### Don't push directly to `dev`, use feature branches.

**Guideline: Use feature/issue branches to contribute work, but do NOT push directly to `main` or `dev`.**

We should stop pushing directly on `dev` or the other "important" branches: Instead, we should always open
a new MR. There are multiple advantages in doing so:

- Reverting work on `dev` is easier, because one can simply revert a single merge commit rather than
  multiple commits;
- We reduce the risk of accidentally _undoing_ work; it already happened in that past that pushing on `dev`
  accidentally deleted some previous work being committed, and that is very painful, especially due to
  tendency to force push into branches.
- The history becomes easier to follow; we can always see which particular MR introduced a particular
  feature or regression;
- New features or bugs won't get missed (see "Review all the things" section).

Typically the argument for pushing directly to `dev` is to make things "quicker", for example to test an
experimental Gargantext feature. However, we should be in a position to deploy any given branch on the
staging server, so that we don't depend on `dev` being the branch we deploy: this reduces the pressure for
pushing directly into `dev`.

### Review all the things.

**Guideline: Whenever possible, we should ask our colleagues to review our work.**

If possible, we should try to review each other's work, because that helps improving the overall quality of
the project. Reviewing is an art in itself; we shouldn't start "cherry-picking" on every single MR, because
that's not the spirit. The purpose of the code review is the following:

- Offering to the colleague a fresh perspective on a problem ("Have you tried X instead of Y"?) which he/she
  might not have thought about;
- Sharing knowledge with the rest of the team (i.e. the fact I'm now the reviewer means I now know about this
  feature, and it won't catch me off guard next time I pull the latest version of the project);

In the spirit of faster development cycles, we shouldn't necessarily veto any MR (unless there are clear bugs),
but if there are concerns with the work being committed, we should share our concerns and perhaps log an issue
in the issue tracker to discuss further work (e.g. MR !XXX added feature Y in a way it might be problematic,
let's keep on eye if this turns out to be a problem.").

To start a review, we should spontaneously pick another colleague and assign him/her as a reviewer in the
appropriate Gitlab UI.

### Do not merge `WIP` merge requests.

**Guideline: if a Merge Request is marked as `WIP`, do not merge it, because it's still a work in progress.**

Gitlab gives the chance of marking a MR as a draft by prepending the text "WIP:" next to the MR's title.
Therefore, we should honour the original author's will by not merging this MR until CI passed _and_ the
`WIP:` prefix has been removed from the title.

### Write test.

**Guideline: try to write a test for each new feature or bug added to Gargantext. If that's hard, it
means you are not developing with testability in mind.**

Testing is arguably the second most important asset of a project (the first being the non-testing code itself),
because it protects us from accidentally introducing regressions in the codebase. You shouldn't believe
the folklore of "if it compiles, it works", because it's simply not true: you need tests. Writing tests
has all sorts of important advantages:

- As said, tests protect against accidentally introducing bugs into existing code;
- They give other developers a sort of "free documentation", because by reading the tests, they
  can learn about the expected behaviour of the code being tested;
- They force the developer to think about "how do I test this?", which is annoying at first, but
  then it encourages more modular code, because:
  * Pure functions are the easiest to test, so that naturally promotes writing as much pure code
    as possible;
  * Effectful polymorphic code (i.e. polymorphic over a constrained monad `m`) is the second easiest
    to test, so that naturally encourages writing lighter monad stacks which can be tested easily, or
    functions over a monad `m` where `m` can be `IdentityT`/`StateT`/`ReaderT`.

### Features development lifecycle.

**Guidelines: each major feature or architectural decision should be discussed with the team.**

For each major feature introduced in Gargantext, there should be a process to propose, discuss and approve
the feature, for several reasons:

- It gives a chance to all developers to chime in on the discussion, bringing in their experience or
  expertise;
- It gives a chance to all developers to understand "what's coming next" without surprised further
  down the road, with features added without they knowing about them;
- It ensures that each new feature or design choice in Gargantext is well understood, principled and
  with a clear purpose;
- It gives us an historical record of precisely _why_ something was done; we can refer to the original
  ticket by saying "this feature/design was discussed here and approved by the team".

Ideally, we could have the following process, divided in 4 phases:

- _Triage_: It starts with a developer opening a new issue in the tracker, describing the feature or the
  design he/she wants to propose. This ticket should be marked with a `triage` label on Gitlab, and it
  should contain:
    * Why such feature or design choice makes sense;
    * Which is the problem it's trying to solve;
    * What is the impact on the rest of the system.
- _Discussion_: Once the ticket has been opened, it's time for other developers to chime in and discuss
  the proposal;
- _Approval_: Once the proposal has been approved, it should be assigned to a developer and it should be
  marked with an `approved` label on Gitlab. The old `triage` label should be removed;
- _Implementation_: Finally, the ticket gets implemented. This concludes the lifecycle.

## Conclusion

We have presented a comprehensive overview on the set of best practices we should put in place within
Gargantext to make sure the project thrives, keeps growing and succeeds.

## Revisions and certification

- To review and agree with these guidelines, read it eventually improve
  it and commit a change as signature.

2023-07-10: Initial version of this document
