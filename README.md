Kachushi
========

An artificial intelligence for the card game Open Faced Chinese Poker.

To build: 

    $ cabal install -O2 --ghc-options='-threaded -with-rtsopts="-N"'
    
To run: 

    $ Kachushi n p
    
where:

* n = Number of players (2, 3, or 4).
* p = Human player's seat (0, 1, 2, or 3). A number out of this range will cause all players to be computer-controlled.

As a human player, to place cards in: 

* the top row, press one of q,w,e,r,t,y,u,i,o,p,
* the middle row, press one of a,s,d,f,g,h,j,k,l
* the bottom row, press one of z,x,c,v,b,n,m


## More Information

See [here](http://scrambledeggsontoast.github.io/2014/06/26/artificial-intelligence-ofcp/).
