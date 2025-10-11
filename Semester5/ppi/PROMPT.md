# prompt

help me create a project charter plan that will respect the following requirements:

```
Theme: To establish a clear vision, define the core innovative element as a testable idea, and plan the
essential development work.

Tasks:
Problem & Solution Statement: In one paragraph, define your target user, the problem you are solving,
and how your product offers a unique solution.

Innovation Hypothesis: State the core innovation of your project as a clear, testable hypothesis.

Example: "We hypothesize that using an AI model to generate personalized workout routines (the innovation) will lead
to higher user engagement (the value) than static, pre-defined plans (the alternative)."

Minimum Viable Feature Set: List the 3-5 absolute essential features required to test your hypothesis.

Technology Stack & Roles: Briefly outline the primary technologies you plan to use (e.g., Python/Flask
for AI, React for web, Swift for iOS) and assign initial roles to the team.
```

the context of the application that i want to build with my team is the following:

we want to build an ai agent chat with a rag system for providing more speical info about specific details about a company. the folloing things need to happen. the agent should provide accurate data from the rag system. it should also provide link to specific documents or pages that will be saved in the db. it should also be able to provide with download links for documents.
lets take for example a uni student. he looks for info about classes, or about proffessors, or about activities, or he looks for documents about erasmus or scholarships. i want the agent to provide the info about what he searcher, the links to special platforms that the student might need to go to that are more specific and harder to find in a traditional way. and i want to provide possible download links for documents that he might need to fill out with a prewview inside the chat. also we should have the capabilities to read a website if the use will provide the link(this will be done with something life firecrawl)
that is the main goal. the app should be built and package in such a way that if i want to change for example the uni theme, for anothr theme, like for a backing client, or for a law client, i should just update the rag system and the docs and the data not the entire system.


the tech stack that we thought of using was either:
- n8n agents with a rag system hosted on the n8n cloud with a react frontend. where we would have multiple agents or workflows for providing the details based on the type of query. like an agent that has access to 3 other agents. the first agent decides to which agent to route the query. and then ther will be specific agents for question related to documenst, or to question that would imply a tutorial on how to use other sepcific platoforms. or for question that will direclty lead to downloading documents and completing them. and so on


- react + convex backend, which also has agentic building capabilities with tool calling and is very well integrated with the ai sdk. i want to use on the frontend the ai elemments ui primitives and shadcn. for payments i am thinking about using polar which is well integrated with convex as well. i am thinking that for the data the agent has to spit out we should use some kind of structured output and some libraries that will facilitate this in order to achieve the best ux and the links to documents and previews

help me make a plan in order for me to decide which is the best route or maybe hopefully if i can integreate both of these options.

this is going to be a web app

important metions:
the ux is very important, that would be one of the differentiator factor. i want to build something similar to the chatgpt experience which provides not just simple text, but diffrent ui componenets based on the query. this dynamic ui is core. it should be completly dynamic. but there should be states like i described above: preives for docs inline or links or tutorials

the development work should be split in sprints of 2 weeks. the first sprint should be focused on the research and the planning of the entire project. the second sprint should be focused on building a prototype with the core features. the third sprint should be focused on testing and gathering feedback. the fourth sprint should be focused on refining and improving the prototype based on feedback. the fifth sprint should be focused on preparing for launch and marketing.

Main theme (proposed by client - Banking)
i want you to tailor the plan using the banking theme as an example, but also keeping in mind that the app should be easily adaptable to other themes like university or law. the core functionalities are the same, i need you to write the examples in the plan from the perspective of a banking client


Output:
provide a plan charter that would answer clearly and compactly to the requirements above and also to the context of the application that i want to build with my team. the response should be writtne in markdown format clearly strcutured and making use of the ui components that markdown provide( like tables list and bolds and italic and inline blocks and code blocks) in order for it to be as clearly as possible
