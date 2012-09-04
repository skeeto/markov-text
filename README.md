# Markov chain text generator

Generates text based on sample input text. Sometimes it produces funny
nonsense.

> He wiped himself again, as if he didn't marry her by hand.

By default, the
[Markov chain](http://en.wikipedia.org/wiki/Markov_chain) is generated
from the text samples in `data/`. The chain is stored in
`markov-text-database` and text is generated with
`markov-text-generate` and `markov-text-insert` (interactive with
prefix-argument).

Given the English input text, the output is filled paragraphs that
look like this,

> Three days to live.
>
> There was absolute silence for several minutes, and then a flare of
> light flashed up, and showed me Orlick.
>
> Whom I had looked for, I don't know.
>
> You said just now that Estella was walking away from me as was the
> sight of me.
>
> Moving the lamp as the man moved, I made out that he was making
> overtures of peace. The throwing down of his weapons and the
> withdrawing of his troop before his advance toward me would have
> signified a peaceful mission anywhere on Earth, so why not, then, on
> Mars!
>
> Placing my hand over my heart I bowed low to the Martian and explained
> to him that while I could catch telepathic messages easily from
> others, and often when they were all formally doomed, and some of them
> were supported out, and some of them were of enormous height; there
> were animals in some of the doorways, but as the natural oil which it
> requires can only be obtained by mining in one of them cut my hair.

Paragraph and sentence structure comes from the states of the Markov
chain itself so there's no direct control over the size of paragraphs
and such.

## Lorem ipsum

Also included is a
[lorem ipsum](http://en.wikipedia.org/wiki/Lorem_ipsum) generator
built on top of this. It maintains its own database independently. The
output of `lorem-ipsum-insert` (interative with prefix-argument) looks
like so,

> Lorem ipsum dolor sit amet, consectetur adipiscing elit. Philosophia
> audeam scribere? Sed ex eo credo quibusdam usu venire; ut abhorreant
> a Latinis, quod inciderint in inculta quaedam et horrida, de malis
> Graecis Latine scripta deterius. quibus ego assentior, dum modo de
> isdem rebus ne Graecos quidem legendos putent. res vero bonas verbis
> electis graviter ornateque dictas quis non legat? nisi qui se plane
> Graecum dici velit, ut a Scaevola est praetore salutatus Athenis
> Albucius.
>
> Quem quidem locum comit multa venustate et omni sale idem Lucilius,
> apud quem praeclare Scaevola:
>
> Iis igitur est difficilius satis facere, qui se Latina scripta dicunt
> contemnere. in quibus hoc primum est in quo admirer, cur in
> gravissimis rebus non delectet eos sermo patrius, cum idem fabellas
> Latinas ad verbum e Graecis expressas non inviti legant. quis enim tam
> inimicus paene nomini Romano est, qui Ennii Medeam aut Antiopam
> Pacuvii spernat aut reiciat, quod se isdem Euripidis fabulis delectari
> dicat, Latinas litteras oderit?
