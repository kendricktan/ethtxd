FROM nixos/nix:2.3.6

RUN nix-env -i cachix git

RUN mkdir /ethtxd

RUN git clone https://github.com/kendricktan/ethtxd.git /ethtxd && cd /ethtxd && git reset --hard 77b56679e1dd27624102e94b3a324bec7c97bc8e

RUN cachix use dapp

RUN cd /ethtxd && nix-shell --command 'cabal v2-build' --pure

RUN cd /ethtxd && cp ./dist-newstyle/build/x86_64-linux/ghc-8.6.5/ethtxd-0.1.0/x/ethtxd/build/ethtxd/ethtxd /usr/bin/ethtxd

CMD [ "ethtxd" ]