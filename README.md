# Deployment of PD2AF

1) Have `docker` and `docker-compose` installed.

2) Add a `.env` file containing `odysseus` and `pd2af` deploy credentials - Ask a maintainer -.

3) Edit the `.env` file to fit server values.

4) `docker-compose up -d`

# Development


The development `compose` file will override container code content provided the fact that you cloned `odysseus` and `pd2af` repository in this directory.

```
# cd deployment (current dir)
git clone ssh://git@git-r3lab-server.uni.lu:8022/elixir/pd2af/odysseus.git
git clone ssh://git@git-r3lab-server.uni.lu:8022/elixir/pd2af/pd2af.git
docker-compose -f docker-compose.yml -f docker-compose-dev.yml up -d
```
