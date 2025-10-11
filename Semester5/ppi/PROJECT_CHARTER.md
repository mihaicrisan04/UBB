
### Problem & Solution Statement
Banking customers and support agents struggle to find precise, up-to-date answers, deep links to hidden portals (e.g., card limits, fee schedules, dispute forms), and downloadable documents (KYC, loan applications) across fragmented systems. Our web app provides an AI agent with RAG that returns accurate, cited banking policy answers, direct deep links to the right platform pages, and inline document previews with secure download links—packaged so swapping to other domains (university, law) only requires updating the RAG corpus and metadata, not the orchestration layer.

### Innovation Hypothesis
We hypothesize that a single orchestrator AI agent, powered by domain-scoped RAG and structured output rendering, will deliver faster, more accurate banking support and higher task completion rates than traditional search or static FAQs by returning dynamic UI blocks (answers, citations, deep links, document previews, and secure downloads).

### Minimum Viable Feature Set
- **RAG Answers with Citations:** Retrieve banking policies/fees/limits with source citations and confidence signals.
- **Deep Link Navigator:** Resolve and return bank portal deep links (settings, dispute forms, FX rates) hard to find normally.
- **Document Preview + Download:** Inline preview (image/PDF) and signed download links for forms (e.g., KYC, loan).
- **Structured Output Rendering:** AI returns typed payloads that map to UI components (text, link, doc_preview, tutorial).
- **On-demand Crawl/Sync (n8n):** Firecrawl ingestion pipeline to refresh documents/pages and update embeddings.

### Technology Stack & Roles
- **Frontend:** React + TypeScript, AI Elements UI primitives, Shadcn; dynamic component rendering from structured outputs.
- **Backend (Core):** Convex (auth, sessions, chat state, storage, tools, orchestrator agent).
- **Workflows (ETL):** n8n (Firecrawl, scheduled refresh, retries, external connectors).
- **Embeddings/Index:** External vector DB (Pinecone/pgvector/Weaviate) + re-ranking.
- **Payments:** Polar integrated with Convex.
- **Roles:**
  - **Product/Orchestration Lead:** Owns agent design, UX flows, structured output schema.
  - **Frontend Engineer:** Implements dynamic chat UI components, state, eval dashboards.
  - **Backend/AI Engineer:** Convex functions, tools, RAG query, vector store, structured outputs.
  - **Workflow Engineer:** n8n pipelines, crawl/ETL, webhooks, retries/schedules.
  - **Designer/UX:** Component states, previews, empty/error/loading patterns, accessibility.

### Architecture (Hybrid: Single Orchestrator)

```text
// Data flow
[React + AI Elements] -> [Convex Orchestrator Agent]
Convex tools:
 • RAG Query -> [Vector DB + Docs Store]
 • Deep Link Resolver -> [Docs/Links metadata in Convex]
 • Document Service -> [Signed URLs + Previews (CDN/S3)]
 • Crawl Request -> [n8n Firecrawl/ETL]
[n8n] -> callback webhooks -> [Convex job_status + updated embeddings + metadata]
```


### Structured Output Schema (example)
```json
{
  "type": "answer_bundle",
  "blocks": [
    { "kind": "text", "content": "Your daily ATM withdrawal limit is 2,500 RON." },
    { "kind": "link", "title": "Card Fees Schedule", "href": "https://bank.example/docs/card-fees.pdf" },
    { "kind": "doc_preview", "docId": "kyc-individual-v3", "title": "KYC Individual Form", "previewUrl": "https://cdn.example/previews/kyc-v3.png", "downloadUrl": "https://cdn.example/downloads/kyc-v3.pdf" }
  ],
  "citations": [
    { "title": "Retail Banking Fees 2025", "href": "https://bank.example/policies/fees-2025" }
  ]
}

Sprint Plan (2-week sprints)
 1. Sprint 1: Research & Planning
 ▫ Finalize orchestrator + tool design, structured output schema, banking taxonomy (fees, limits, KYC).
 ▫ Pick vector DB, define chunking/embedding strategy, security model (tenants/roles).
 ▫ Map n8n crawl pipelines (Firecrawl, HTML→MD, chunk, embed).
 2. Sprint 2: Prototype Core
 ▫ Implement Convex orchestrator agent + tools (RAG, DeepLink, Document).
 ▫ Frontend chat with AI Elements + Shadcn rendering dynamic blocks.
 ▫ Minimal n8n ingestion pipeline; seed banking docs/links.
 3. Sprint 3: Testing & Feedback
 ▫ Accuracy evals (retrieval/citation correctness), latency tests, UX feedback on component states.
 ▫ Secure downloads (signed URLs), edge cases (missing docs, expired links).
 4. Sprint 4: Refinement
 ▫ Improve ranking, add re-ranker, harden prompts, better error/loading skeletons.
 ▫ Enhance deep link resolver and metadata quality, add tutorials block type.
 5. Sprint 5: Launch Prep & Marketing
 ▫ Production hardening (observability, retries, rate limits), pricing via Polar.
 ▫ Demo content, metrics dashboard (CSAT, task completion), docs and walkthrough video.

Adaptability (University/Law)

Keep the same orchestrator + tools; just swap RAG corpora and metadata:
 • University: Classes, professors, Erasmus/scholarships docs, deep links to academic portals.
 • Law: Statutes, case summaries, client intake forms, deep links to court/registry portals.

-------------------------------

ultra-simple: Convex-only chat + manual docs; add n8n later for crawl.

Beginner-friendly plan (minimal, realistic)

Goals

Build a usable web chat that answers domain questions from a small curated corpus, returns deep links, and shows document previews/downloads. Keep scope tiny and ship fast.

Tech (simple defaults)
 • Frontend: Next.js + React + TypeScript, Shadcn UI.
 • AI/UI: AI Elements (or Vercel AI SDK) with structured outputs.
 • Backend: Convex (auth, DB, server functions, single orchestrator agent).
 • RAG (starter): Store chunked text + embeddings in Convex; use OpenAI/Anthropic embeddings API; basic cosine search.
 • Storage: S3 (or Supabase Storage) for PDFs + preview images; signed URLs via Convex.
 • Deploy: Vercel (frontend) + Convex cloud.
 • Optional later: n8n for crawling + ETL when you have time.

Roles (for 3–4 students, busy schedule)
 • Product/Orchestrator: Owns prompts, tools, output schema; 3–4 h/week.
 • Frontend: Chat UI + components (text, link, doc_preview); 4–6 h/week.
 • Backend: Convex functions (RAG query, link resolver, doc service); 4–6 h/week.
 • Content/ETL (part‑time): Curate docs/links, make previews; 2–3 h/week.

Scope you can finish
 • Manual corpus first: 20–50 pages/forms copied to Markdown, chunked by headings.
 • Three tools: RAGQuery, DeepLinkResolver, DocumentService (preview + download).
 • Structured output: Small schema that maps to UI blocks.

{
  "type": "bundle",
  "blocks": [
    { "kind": "text", "content": "Summary answer..." },
    { "kind": "link", "title": "Fees Schedule", "href": "https://example/fees.pdf" },
    { "kind": "doc_preview", "docId": "kyc-v1", "title": "KYC Form", "previewUrl": ".../kyc.png", "downloadUrl": ".../kyc.pdf" }
  ],
  "citations": [{ "title": "Policy 2025", "href": "https://example/policy" }]
}

2-week sprints (lightweight)
 • Sprint 1 (Plan + Corpus): Define schema, set up Convex/Next.js, collect 20–50 docs/links, chunk + embed manually.
 • Sprint 2 (Prototype): Orchestrator agent + 3 tools; dynamic UI renders blocks; signed downloads working.
 • Sprint 3 (Test/Feedback): Accuracy spot-checks, latency, empty/error/loading states; fix top 10 issues.
 • Sprint 4 (Refine): Better ranking, stricter citations, add “tutorial” block type; polish UX.
 • Sprint 5 (Prep): Basic metrics (task completion, feedback form), README + demo; optional n8n crawl pilot.
