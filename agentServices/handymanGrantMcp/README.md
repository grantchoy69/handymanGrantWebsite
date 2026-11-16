# Handyman Grant MCP staging

This standalone Node service lives in its own directory and deployment branch. It does not modify the public website, landing-page generator, admin application, Supabase schema, or the existing Kacey Render services.

## Render plan

- Workspace: My Workspace
- New project: Handyman Grant MCP
- New environment: staging
- New web service: handyman-grant-mcp-staging (Render may append a unique suffix)
- Compute: free, Oregon, one instance
- Root directory: agentServices/handymanGrantMcp
- Build: npm ci --ignore-scripts && npm run check && npm test
- Start: npm start
- Health: /healthz
- MCP: /mcp (stateless Streamable HTTP with JSON responses)
- Real quote submissions: disabled
- Automatic deploys and Blueprint auto-sync: disabled

Create the Blueprint from this branch and agentServices/handymanGrantMcp/render.yaml using create_new, not adopt. Inspect the proposed changes before approval. No existing service should be modified or deleted.

## Boundaries

The four tools are getBusinessInfo, searchServices, prepareQuoteRequest, and submitQuoteRequest. Public information and quote drafting work in staging. submitQuoteRequest refuses writes while QUOTE_SUBMISSIONS_ENABLED=false. No customer/job/database read tool exists. No Supabase service-role or OpenAI API key is needed.

Render generates DRAFT_ENCRYPTION_SECRET. Never put it into source control. Draft tokens contain encrypted customer-provided information and must not appear in logs or URLs.

This is deployment separation, not a cloned Supabase database or a claim of private-network isolation. The configured intake URL points at the existing Supabase intake endpoint, but staging must remain submission-disabled. Do not turn submissions on until a separate review and explicitly approved end-to-end test.

## Verification

Run npm run check and npm test. All 29 tests passed locally before publishing; submission tests use a mock backend and create no real leads.

After deployment, run npm run smoke -- https://ACTUAL-RENDER-HOST/mcp. The smoke test initializes MCP, discovers tools and reads public business information only. A real AI-client interoperability test remains required before release.

## Pilot limits

Free compute can sleep. Submission limits are in-memory and per process; keep one instance. All server-to-server leads can share Render egress, so the existing intake's IP limit is shared. Publishing an endpoint does not automatically distribute it to AI users. Production authentication/distribution and durable abuse controls are separate work.
