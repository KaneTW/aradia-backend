An implementation of the Discord API

Goals:
* Remove as much partiality as possible. ExceptT instead of IO exceptions, no `error`, etc etc
* Type-safe API similar to Servant (likely: use Servant for backend)
* Caching support (with proper isolation of servers)
* Eventually: pluggable caches (in-memory, Redis, ...)
* Ideally pluggable events --- user should be able to add a missing endpoint or whatever. Probably not reasonably possible.
* Proper separation of servers (guilds) --- user data should not leak from one to the other
* Websocket stuff, eventually voice
* Storage backends are probably out of scope for this?
* Honor rate limits
* Properly handle all discord errors
* Properly handle authentication. Eventually: Full OAuth2 flow support
* Be compatible with an eventually consistent system
``` Due to this, client actions can never be serialized and may be executed in any order (if executed at all). Along with these constraints, events in Discord may:
Never be sent to a client
Be sent exactly one time to the client
Be sent up to N times per client
Clients should operate on events and results from the API in as much of a idempotent behavior as possible.
```
* Properly handle user-agent
