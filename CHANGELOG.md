# Changelog for `ai-rake`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

- Migrated Gemini Interactions chat requests, responses, and streams to the current typed-step schema while retaining legacy stored-response decoding.
- Changed canonical conversation storage to an append-only agent log. Provider-native items now carry `ItemPending` or `ItemCompleted`, and unresolved tool calls or incomplete assistant output are persisted as ordinary history instead of being hidden in transient loop state.
- Replaced `chatOutcomeFinalizedItems` with `chatOutcomeItems` as the low-level storage extractor for resumable `chatOutcome` flows.
- Added embedded `HistoryItemId`s plus `ResetCheckpoint`-based recovery helpers for append-only replay, including `validResetCheckpoints`, `latestValidCheckpoint`, `resetToLatestValidCheckpoint`, `resetTo`, and `resetToStart`.
- Provider-backed history items now store the local `ToolDeclaration`s that were available to the model for the request that produced the provider item.
- `chat` and `chatOutcome` now require `IOE` so the library can assign `HistoryItemId`s before replay and persistence.
- Added durable replay barriers for failed `chatOutcome` runs, and a typed `ConversationBlocked` error for strict `chat` callers.
- Historical unresolved tool calls now resume into a synthetic `"Tool not found"` result when the local tool is no longer configured, instead of being silently dropped.
- Added broader shared loop tests plus deterministic OpenAI/xAI/Gemini decoder tests for completed, tool-handoff, paused, failed, and thought-only rounds.
- Added provider-specific media generation helpers for OpenAI Images (`gpt-image-1.5` by default) and xAI Grok Imagine image/video generation.
- Added a `rake-image` CLI for OpenAI and Grok image generation, a separate `rake-video` CLI for Grok video generation from an image, video edits, and local append-style `--extend` continuations, and a `rake-tts` CLI for OpenAI/xAI speech generation with local playback by default and optional `--output` saving.
- Moved PostgreSQL response logging onto the same effectful `WithConnection` abstraction as the storage backend.
- Replaced raw PostgreSQL table-name `Text` with validated `PgIdentifier` and `ConversationTables` configuration.
- Added batched appends plus `modifyConversationAtomic` for short-lived concurrent storage mutations.
- Made OpenAI/xAI media helpers fail fast on unsupported option combinations instead of silently dropping request fields.
- Split the large shared Responses and xAI Imagine implementation modules into smaller internal units.

## 0.1.0.0 - YYYY-MM-DD
