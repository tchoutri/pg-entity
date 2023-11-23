# Contributing #

## Setting Up a Development Environment ##

### Nix ###

If you know and use nix, we recommend using our `flake.nix` file.

All you have to do is

    $ nix develop

#### Nix Tips for Beginners ####

Get setup with Nix: `insert-url-here`

Start the Nix daemon if the install tells you that it's not compatible
with your init system:

    $ nix daemon

If this is your first time setting up Nix you'll want to use the
`flakes` feature and not have to enable on the CLI every time:

    $ echo 'experimental-features = nix-command flakes' >> ~/.config/nix/nix.conf

This works for the multi-user setup and the single-user setup.

## Running Tests ##

Once your development environment is setup you should be able to use
the `test` target in the Makefile to run `./scripts/run-tests.sh`
which will source `environment.sh`, connect to the PostgreSQL database
from there, and setup and run the integration tests:

   $ <pg-entity> make test
