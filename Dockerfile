FROM nixos/nix:2.3.6

RUN mkdir /ethtxd

RUN wget https://github.com/kendricktan/ethtxd/releases/download/v0.1.0/ethtxd-x86_64-linux -O /usr/bin/ethtxd

RUN chmod +x /usr/bin/ethtxd

COPY nix /ethtxd/nix

COPY default.nix /ethtxd

COPY shell-bin.nix /ethtxd

RUN cd /ethtxd && nix-shell shell-bin.nix

CMD [ "/bin/sh", "-c", "cd /ethtxd && nix-shell shell-bin.nix --command '/usr/bin/ethtxd'" ]