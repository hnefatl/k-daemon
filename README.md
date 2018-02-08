# k-daemon

Daemon for running a restricted set of commands with elevated privileges. Only intended for personal use, but easily modified to support arbitrary commands.

## Current Commands

- `k-daemon apt-update`: Performs `apt update`, and returns the same exit code.
- `k-daemon vpn`:
  - `status`: Performs `ipsec status`, which prints the VPN connection information among other things.
  - `up <name>`: Performs `ipsec up <name>`, attempting to connect to the given VPN.
  - `down <name>`: Performs `ipsec down <name>`, attempting to disconnect from the given VPN.