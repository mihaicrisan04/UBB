# Simplified AI Banking Assistant - Project Charter

## Problem & Solution Statement

Banking customers and support agents waste valuable time navigating fragmented systems to find specific information like account limits, fee schedules, dispute procedures, and downloadable forms (KYC documents, loan applications). Our **AI Banking Assistant** provides a conversational interface powered by RAG (Retrieval-Augmented Generation) that delivers accurate, cited answers, direct deep links to banking portals, and inline document previews with secure downloads—all through a dynamic, ChatGPT-like UI that adapts to different industries by simply updating the knowledge base.

## Innovation Hypothesis

**We hypothesize that a single AI agent with domain-specific RAG and structured output rendering will achieve higher customer satisfaction and faster task completion rates than traditional banking search or static FAQs by delivering dynamic UI components (answers, links, document previews, tutorials) in a conversational format.**

## Minimum Viable Feature Set

### Core Features (Must-Have)
1. **RAG-Powered Q&A** - Accurate banking policy answers with source citations
2. **Deep Link Navigation** - Direct links to specific banking portal pages (card settings, dispute forms, rate schedules)
3. **Document Preview & Download** - Inline preview of forms/documents with secure download capabilities
4. **Dynamic UI Components** - Structured responses that render as different UI blocks based on query type
5. **Website Crawling** - Ability to ingest new content from provided URLs using Firecrawl

### Example Use Cases (Banking Theme)
- *"What's my daily ATM withdrawal limit?"* → Answer + link to card settings
- *"I need to file a dispute"* → Tutorial + dispute form preview + download link  
- *"Show me current exchange rates"* → Live rates + link to FX portal
- *"I need KYC documents"* → Form preview + download + completion tutorial

## Technology Stack & Roles

### **Recommended Architecture: React + Convex (Simplified)**

| **Technology** | **Purpose** | **Why This Choice** |
|----------------|-------------|-------------------|
| **Frontend** | Next.js + React + TypeScript | Fast development, great AI SDK integration |
| **UI Components** | AI Elements + Shadcn | Pre-built chat components, consistent design |
| **Backend** | Convex | Built-in AI tools, real-time sync, simple deployment |
| **RAG System** | Convex DB + OpenAI Embeddings | Integrated storage, no external vector DB needed |
| **File Storage** | Convex File Storage | Seamless integration, signed URLs |
| **Payments** | Polar + Convex | Native integration for monetization |
| **Content Ingestion** | Firecrawl API | Simple website crawling and content extraction |

### **Team Roles & Time Commitment**

| **Role** | **Responsibilities** | **Time/Week** |
|----------|---------------------|---------------|
| **Product Lead** | Agent design, UX flows, structured output schema | 3-4 hours |
| **Frontend Engineer** | Chat UI, dynamic components, user experience | 4-6 hours |
| **Backend Engineer** | Convex functions, RAG system, AI agent orchestration | 4-6 hours |
| **Content Curator** | Document collection, testing, quality assurance | 2-3 hours |

### **Structured Output Schema**
```json
{
  "type": "response_bundle",
  "blocks": [
    { 
      "kind": "text", 
      "content": "Your daily withdrawal limit is $500." 
    },
    { 
      "kind": "link", 
      "title": "Modify Card Settings", 
      "href": "https://bank.example/cards/settings" 
    },
    { 
      "kind": "document", 
      "title": "Dispute Form", 
      "previewUrl": "/previews/dispute-form.png",
      "downloadUrl": "/secure/dispute-form.pdf" 
    }
  ],
  "citations": [
    { 
      "source": "Banking Policies 2024", 
      "url": "https://bank.example/policies" 
    }
  ]
}
```

## Sprint Plan (2-Week Sprints)

### **Sprint 1: Research & Foundation** 📋
- **Goals**: Architecture decisions, setup, content strategy
- **Deliverables**:
  - Finalize tech stack (Convex vs n8n comparison)
  - Define banking content taxonomy (accounts, cards, loans, investments)
  - Set up development environment
  - Create structured output schema
  - Collect initial 20-30 banking documents/policies

### **Sprint 2: Core Prototype** 🏗️
- **Goals**: Build working MVP with basic features
- **Deliverables**:
  - Convex backend with AI agent setup
  - Basic chat interface with AI Elements
  - RAG system with banking content (embedded and searchable)
  - Simple document preview and download functionality
  - Dynamic UI rendering for different response types

### **Sprint 3: Testing & Refinement** 🧪
- **Goals**: Validate hypothesis, gather feedback
- **Deliverables**:
  - Accuracy testing (correct answers, relevant citations)
  - UX testing with banking scenarios
  - Performance optimization (response times < 3 seconds)
  - Error handling and edge cases
  - User feedback collection system

### **Sprint 4: Enhancement & Polish** ✨
- **Goals**: Improve based on feedback, add advanced features
- **Deliverables**:
  - Improved RAG ranking and relevance
  - Enhanced deep link resolution
  - Better document preview experience
  - Tutorial/walkthrough block type
  - Loading states and error handling

### **Sprint 5: Launch Preparation** 🚀
- **Goals**: Production readiness and go-to-market
- **Deliverables**:
  - Production deployment and monitoring
  - Pricing integration with Polar
  - Demo content and marketing materials
  - Documentation and user guides
  - Success metrics dashboard

## Multi-Domain Adaptability

### **Core System (Unchanged)**
- Chat interface and dynamic UI components
- RAG architecture and search functionality  
- Document preview and download system
- Agent orchestration and tool calling

### **Domain-Specific Elements (Swappable)**

| **Domain** | **Content Examples** | **Deep Links** | **Documents** |
|------------|---------------------|----------------|---------------|
| **Banking** | Account limits, fees, policies | Card settings, dispute forms | KYC forms, loan applications |
| **University** | Course info, professor details | Student portals, registration | Erasmus forms, scholarship applications |
| **Legal** | Case law, statutes, procedures | Court portals, filing systems | Client intake forms, legal templates |

## Success Metrics

### **Primary KPIs**
- **Task Completion Rate**: % of queries resolved without escalation
- **Response Accuracy**: % of answers with correct citations
- **User Satisfaction**: Rating after each interaction
- **Time to Resolution**: Average time from query to complete answer

### **Technical Metrics**
- Response latency < 3 seconds
- Document download success rate > 95%
- Deep link accuracy > 90%
- System uptime > 99.5%

## Risk Mitigation

| **Risk** | **Probability** | **Impact** | **Mitigation Strategy** |
|----------|----------------|------------|----------------------|
| RAG accuracy issues | Medium | High | Extensive testing, human review process |
| Complex UI state management | Medium | Medium | Use proven state management patterns |
| Content ingestion challenges | Low | Medium | Start with manual curation, automate later |
| Performance bottlenecks | Low | High | Load testing, caching strategies |

---

*This simplified charter focuses on delivering a working prototype quickly while maintaining the flexibility to scale and adapt to different domains. The banking theme provides concrete examples while keeping the architecture domain-agnostic.*