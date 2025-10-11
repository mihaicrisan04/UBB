Set up a pnpm monorepo: Next.js + Convex (frontend/backend) and a Python Docling ingester.

Monorepo setup overview

Keep Convex inside the web app for simplicity, expose one HTTP action for ingestion, and let the Python project push chunks/embeddings to the same Convex instance. This keeps deploys clean and team-friendly.

Repository layout
 • apps/web: Next.js + Convex (schema, functions, actions), Shadcn, AI Elements.
 • tools/ingest: Python (Docling + OpenAI embeddings), batch uploader.
 • packages/ui: shared UI primitives/components.
 • packages/shared: types/schemas (Zod), structured output contracts.

Environment model
 • Root .env with shared keys; apps read per-app envs.
 • Convex cloud holds deployment; web uses Convex client; Python hits Convex HTTP action with a bearer token.

Team workflow
 • Frontend focuses on dynamic blocks, chat UX.
 • Backend owns Convex schema/functions (RAGQuery, DeepLinkResolver, DocumentService, Ingest HTTP action).
 • Python team curates docs, runs Docling/OCR, batches uploads.
 • Shared package owns structured output schema and payload types.

Copy-paste project scaffold# Monorepo files & folders

```txt
.
├─ package.json              # pnpm workspace root (JS)
├─ pnpm-lock.yaml
├─ pnpm-workspace.yaml
├─ .env                      # shared env (safe values only)
├─ .gitignore
├─ apps/
│  └─ web/
│     ├─ package.json
│     ├─ next.config.ts
│     ├─ src/
│     │  ├─ app/            # Next.js App Router
│     │  ├─ components/     # Shadcn + AI Elements blocks
│     │  └─ lib/            # Convex client setup, fetchers
│     ├─ convex/            # Convex lives here
│     │  ├─ schema.ts
│     │  ├─ actions/ingest.ts
│     │  ├─ queries/search.ts
│     │  ├─ functions/tools.ts
│     │  └─ _generated/     # Convex codegen
│     └─ .env.local         # frontend-only secrets (NEXT_PUBLIC_* + Convex URL)
├─ packages/
│  ├─ ui/
│  │  ├─ package.json
│  │  └─ src/index.ts       # shared components
│  └─ shared/
│     ├─ package.json
│     └─ src/schema.ts      # Zod schemas for structured outputs, ingest payloads
└─ tools/
   └─ ingest/
      ├─ pyproject.toml     # Poetry or uv
      ├─ .env               # Python-only secrets (OpenAI keys, ingest URL, bearer)
      └─ ingest.py          # Docling parse → chunk → embed → upload
```

```json
// package.json (root)
{
  "name": "rag-agents-monorepo",
  "private": true,
  "workspaces": ["apps/*", "packages/*"],
  "packageManager": "pnpm@9.0.0",
  "scripts": {
    "dev": "pnpm --filter @app/web dev",
    "build": "pnpm -r build",
    "convex:deploy": "pnpm --filter @app/web convex deploy",
    "convex:codegen": "pnpm --filter @app/web convex codegen"
  }
}
```

# pnpm-workspace.yaml
packages:
  - "apps/*"
  - "packages/*"

```ts
// apps/web/convex/schema.ts
import { defineSchema, defineTable, s } from "convex/schema";

export default defineSchema({
  documents: defineTable({
    docId: s.string(),
    title: s.string(),
    href: s.string(),
    category: s.string(),
    createdAt: s.number()
  }).index("by_docId", ["docId"]),
  chunks: defineTable({
    docId: s.string(),
    chunkId: s.string(),
    text: s.string(),
    embedding: s.array(s.number()),
    position: s.number(),
    href: s.string(),
    page: s.optional(s.number())
  }).index("by_chunkId", ["chunkId"])
});
```
```ts
// apps/web/convex/actions/ingest.ts
import { httpAction } from "convex/server";
import { v } from "convex/values";
import { internal } from "../_generated/api";

export const post = httpAction({
  args: {
    doc: v.object({
      docId: v.string(),
      title: v.string(),
      href: v.string(),
      category: v.string(),
    }),
    chunks: v.array(v.object({
      chunkId: v.string(),
      text: v.string(),
      embedding: v.array(v.float64()),
      position: v.number(),
      href: v.string(),
      page: v.optional(v.number()),
    })),
    bearer: v.string()
  },
  handler: async (ctx, args) => {
    if (args.bearer !== process.env.INGEST_BEARER) {
      return new Response("Unauthorized", { status: 401 });
    }
    await ctx.runMutation(internal.ingest.upsertDocAndChunks, args);
    return new Response(JSON.stringify({ ok: true }), { status: 200 });
  }
});
```
```ts
// apps/web/convex/internal/ingest.ts
import { internalMutation } from "convex/server";

export const upsertDocAndChunks = internalMutation(async (ctx, { doc, chunks }) => {
  const existing = await ctx.db.query("documents").withIndex("by_docId", q => q.eq("docId", doc.docId)).first();
  if (!existing) await ctx.db.insert("documents", { ...doc, createdAt: Date.now() });
  for (const c of chunks) {
    const exists = await ctx.db.query("chunks").withIndex("by_chunkId", q => q.eq("chunkId", c.chunkId)).first();
    if (!exists) await ctx.db.insert("chunks", { docId: doc.docId, ...c });
  }
});
```
```ts
// apps/web/convex/queries/search.ts
import { query } from "convex/server";
import { v } from "convex/values";

function cosine(a: number[], b: number[]) {
  let dot = 0, na = 0, nb = 0;
  for (let i = 0; i < a.length; i++) { dot += a[i]*b[i]; na += a[i]*a[i]; nb += b[i]*b[i]; }
  return dot / (Math.sqrt(na) * Math.sqrt(nb) + 1e-10);
}

export const topK = query({
  args: { queryEmbedding: v.array(v.float64()), k: v.number() },
  handler: async (ctx, { queryEmbedding, k }) => {
    const all = await ctx.db.query("chunks").collect();
    const scored = all.map(c => ({ ...c, score: cosine(queryEmbedding, c.embedding) }));
    scored.sort((x, y) => y.score - x.score);
    return scored.slice(0, k).map(({ chunkId, text, href, docId, score }) => ({ chunkId, text, href, docId, score }));
  }
});
```
```toml
# tools/ingest/pyproject.toml (Poetry or uv; example Poetry)
[tool.poetry]
name = "docling-ingest"
version = "0.1.0"
description = "Docling → embeddings → Convex uploader"
authors = ["Team"]

[tool.poetry.dependencies]
python = "^3.11"
docling = "*"
openai = "^1.52.0"
requests = "^2.32.0"
python-dotenv = "^1.0.1"

[tool.poetry.scripts]
ingest = "ingest:main"
```
```env
# tools/ingest/.env
OPENAI_API_KEY=sk-...
CONVEX_INGEST_URL=https://<your>.convex.site/actions/ingest
INGEST_BEARER=some-long-random-token
```
```py
# tools/ingest/ingest.py
import os, re, requests
from dotenv import load_dotenv
from openai import OpenAI
from docling import DocumentConverter

load_dotenv()
OPENAI_API_KEY = os.environ["OPENAI_API_KEY"]
CONVEX_INGEST_URL = os.environ["CONVEX_INGEST_URL"]
INGEST_BEARER = os.environ["INGEST_BEARER"]

client = OpenAI(api_key=OPENAI_API_KEY)

def clean_text(t: str) -> str:
  return re.sub(r"\s+", " ", t).strip()

def chunk_text(t: str, max_chars=1200, overlap=200):
  chunks, start = [], 0
  while start < len(t):
    end = min(start + max_chars, len(t))
    chunks.append(t[start:end])
    start = max(0, end - overlap)
  return chunks

def embed_batch(texts):
  resp = client.embeddings.create(model="text-embedding-3-small", input=texts)
  return [e.embedding for e in resp.data]

def convert_to_markdown(path: str) -> str:
  md = DocumentConverter().convert(path).export_to_markdown()
  return md

def upload(docId, title, href, category, chunks_text, embeddings):
  payload = {
    "doc": {"docId": docId, "title": title, "href": href, "category": category},
    "chunks": [{
      "chunkId": f"{docId}#{i:03d}",
      "text": chunks_text[i],
      "embedding": embeddings[i],
      "position": i,
      "href": href
    } for i in range(len(chunks_text))],
    "bearer": INGEST_BEARER
  }
  r = requests.post(CONVEX_INGEST_URL, json=payload, timeout=60)
  r.raise_for_status()
  print(f"Ingested {docId} ({len(chunks_text)} chunks)")

def main():
  path = "docs/fees_2025.pdf"
  md = convert_to_markdown(path)
  cleaned = clean_text(md)
  parts = chunk_text(cleaned)
  embeds = embed_batch(parts)
  upload(
    docId="fees-2025",
    title="Retail Banking Fees 2025",
    href="https://bank.example/policies/fees-2025",
    category="fees",
    chunks_text=parts,
    embeddings=embeds
  )

if __name__ == "__main__":
  main()
```
```env
# root .env (non-secret example; secrets per app)
NEXT_PUBLIC_CONVEX_URL=https://<your>.convex.site
INGEST_BEARER=some-long-random-token
```
```ts
// apps/web/src/lib/convexClient.ts
import { ConvexReactClient } from "convex/react";
export const convex = new ConvexReactClient(process.env.NEXT_PUBLIC_CONVEX_URL!);
```

CI/CD (GitHub Actions)
 - Web: build Next.js, run convex deploy and codegen on push to main.
 - Python: a manual “Run ingestion” workflow to process docs when needed.

Roles (minimal)
 - Backend: Convex schema/functions + ingestion endpoint security.
 - Frontend: Dynamic chat blocks and UX; structured output renderer.
 - Python ETL: Docling parsing, embeddings, batch uploader, corpus hygiene.
 - QA: Spot-check retrieval accuracy, broken links, preview rendering.

Notes
 - Keep the live chat path inside Convex; Python only ingests offline.
 - Use bearer token + idempotent chunkIds for safe re-ingests.
 - If corpus grows, swap the cosine loop for a vector DB later—no UI changes needed.
