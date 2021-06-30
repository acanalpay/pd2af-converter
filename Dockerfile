FROM racket/racket:7.7-full
# FROM ubuntu
RUN raco pkg install sha
RUN raco pkg install sxml

ARG ODYSSEUS_USERNAME
ARG ODYSSEUS_TOKEN
ARG PD2AF_USERNAME
ARG PD2AF_TOKEN

WORKDIR /srv
# RUN git clone https://$PD2AF_USERNAME:$PD2AF_TOKEN@git-r3lab.uni.lu/elixir/pd2af/pd2af.git
# RUN git clone https://$ODYSSEUS_USERNAME:$ODYSSEUS_TOKEN@git-r3lab.uni.lu/elixir/pd2af/odysseus.git /srv/pd2af/server/libs/odysseus

COPY . /srv
CMD ["racket",  "/srv/pd2af/code/start-server.rkt"]