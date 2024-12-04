**CASEY**: Thanks [@unclebobmartin](https://twitter.com/unclebobmartin) for taking the time to answer these questions! Maybe we can start with just some clarification.

Most explanations on Clean Code I have seen from you include all the things I mentioned in [the video](https://www.youtube.com/watch?v=tD5NrevFtbU) - preferring inheritance hierarchies to if/switch statements, not exposing internals (the "Law of Demeter"), etc. But it sounds like you were surprised to hear me say that. Could you take a minute before we get started to explain more fully your ideas on type design, so I can get a sense for where the disconnect is here?

---

**BOB**: The disconnect.  Hmmm.  I’m not sure there is one.

I watched the first half of your video.  After that I figured that I had caught the drift.  I responded in one thread that I thought your analysis was essentially correct. I also thought that your rhetoric was a bit inaccurate in representing “clean code”.  I don’t remember exactly what that inaccuracy was; and it doesn’t really matter.

So…. Yes, absolutely, the structures you were presenting are not the best way to squeeze every nanosecond of performance out of a system. Indeed, using those structures can cost you a lot of nanoseconds.  They are not efficient at the nanosecond level.  Long ago this would have been generally important.  We worried about the cost of function call overhead and indirection.  We even unwound loops if we could.  This was especially true in embedded real time environments.

But the kinds of environments where that kind of parsimony is important are nowadays few and far between.  The vast majority of software systems require less than 1% of a modern processor’s power. What’s more, processors are so cheap and available that it is a trivial matter to add more of them to a system. These facts change the trade-off away from program performance to the performance of the development team, and their ability to create systems, and keep those systems running. And it is those needs where the ideas of Clean Code come into play.

It is economically better for most organizations to conserve programmer cycles than computer cycles. So if there is a disconnect between us, I think it is only in the kinds of contexts that we prioritize.  If you are trying to squeeze every nanosecond from a battery of GPUs, then Clean Code may not be for you; at least in the most taxing of your deepest inner loops.  On the other hand, if you are trying to squeeze every man-hour of productivity from a software development team, then Clean Code can be an effective strategy towards that end.

---

**CASEY**: Just to get a little bit more concrete, so I understand what you're saying here, can we pick some specific software examples? For instance, I would assume we are both familiar with using Visual Studio and CLANG/LLVM. Would those both be reasonable examples of what you are calling the vast majority of software that requires less than 1% of a modern processor?

---

**BOB**: No, it seems to me that an IDE is a very specialized software system.  There are only a few in existence, and only a very few that have become popular.

IDE's are interesting systems in that they span a huge domain of contexts.  There are portions in which nanoseconds are extremely important, and other parts where they matter very little.  A modern IDE has to be able to parse large extents of code on a keystroke by kestroke basis.  Making sure that the parsing code preserves nanoseconds can have a big effect.  On the other hand, the code that sets up a configuration dialog does not need even a tiny fractioin of that kind of efficiecy.

And, as an aside, the kind of efficiency the compile engine of an IDE needs is more algorithmic than cycle-lean.  Cycle-lean code can increase efficiency by an order of magnitude; but the right choice of algorithm can increase efficiency by many orders of magnitude.

No, the kind of software I was referring to that requires <1% of a modern processor are the regular run-of-the-mill systems that most programmers engage in.  A website, a calendar app, a process control dashboard (for a simple process).  In fact, just about any Rails app, or any Python or Ruby app.  Even most Java apps.  You simply would not choose languages like that if nanoseconds were your concern.

A language like Clojure (my "go to" language at the moment) is likely 30X slower than an equivalent Java app, and probably 60X slower than an equivalent C app.  But I don't care that much.  First, I can drop into Java if I have to (and I have for compute bound tasks).  Second, for many apps, adding processors is simple and cheap.  And so I usually find the programmer time saving to be cost effective.

Don't get me wrong.  I'm an old assembler and C hacker from the 70s and 80s.  I assiduously counted microseconds when it mattered (nanoseconds were way beyond anything we could imagine).  So I know how important cycle-lean code can be.  But today's processors are 10,000 times faster than the machines we used in those days (literally).  So for most software systems nowadays we have the ability to trade some of those extra cycles per second for programmer efficiency.

---

**CASEY**: If I understand you correctly, you are saying there are two broad categories of software, so we perhaps have to discuss each one separately. It sounds like most software I actually use falls into the category where "nanoseconds can matter", in your terminology - in other words, Visual Studio, LLVM, GCC, Microsoft Word, PowerPoint, Excel, Firefox, Chrome, fmmpeg, TensorFlow, Linux, Windows, MacOS, OpenSSL, etc. I assume based on your answer that you would agree all of those do have to be concerned about performance?

---

**BOB**: Not exactly.  Rather my experience is that there is a broad spectrum of software that applies at the module level.  Some modules must perform with nanosecond deadlines.  Others require microsecond response times.  Still others need only operate under millisecond constraints.  And there are modules that would operate satisfactorily with response times approaching a second.

Most applications are constructed of modules that cover much of this spectrum.  For example, Chrome must render quickly.  Microseconds matter when you are populating a complex web page.  On the other hand, The preferences dialog in Chrome likely doesn't even need to be responsive at the millisecond level.

If we thought of the modules within a particular application arranged into a histogram by response time, I presume we'd see some kind of non-normal distribution.  Some applications may have many nanosecond modules and few millisecond modules.  Other applications would have most modules in the millisecond range and few in the nanosecond range.

For example, I'm currently working on an application in which the vast majority of modules work well at the millisecond level; but a few require 20X better performance.  My strategy has been to write the millisecond modules in Clojure because, while slowish, it is a very convenient language.  The microsecond modules I wrote in Java which is much faster, but far less convenient.

There are languages, and structures, that abstract away the hard metal of the machine and make it easier for programmers to focus on the problem domain.  It is far more efficient for a programmer to write millisecond level code when they don't have to be concerned with optimizing L2 cache hits.  They can think, instead, about the business requirements, and about the other programmers who will have to deal with their code over the next decade.

There are also languages and structures that expose the hard metal of the machine to make it easier for programmers to squeeze every last nanosecond out of an algorithm.  Those structures may not be the easiest to write, explain, or maintain; but when nanoseconds count it would be folly to ignore them.

And of course there are languages and environments in the middle of that range as well.  Knowing these environments, and knowing which is best for the problem at hand, is something we all need to be proficient at.

***

I wrote a book a decade or so ago entitled _Clean Code_. It focused more on the millisecond side of the problem than on the nanosecond side.  It seemed to me, at the time, and indeed still today, that the problem of programmer productivity was an important issue.  However, the book was not myopic about the issues that you and I are discussing here.  For example, if you read the section that starts with "Prefer polymorphism..." you will see a discussion about the benefits of switch statements in certain contexts.  Indeed, there's whole chapter about such contexts.  That chapter includes the following statement, which I think captures the sentiments I have been expressing in this discussion: "Mature programers know that the idea that everything is an object is a myth. Sometimes you really do want simple data structures with procedures operating on them."

---

**CASEY**: I wanted to be specific here, so I'll try rephrasing my question. Are Visual Studio, LLVM, GCC, Microsoft Word, PowerPoint, Excel, Firefox, Chrome, fmmpeg, TensorFlow, Linux, Windows, MacOS, and OpenSSL examples of programs where "milliseconds matter" _in at least some of their "modules"_, in your terminology?

---

**BOB**: Milliseconds?  Of course.  I'd say that they all have modules where microsecond matter; and many have modules where nanoseconds matter.

---

**Bob**: (The next day) By the way, I went back and watched the entirety of your video the other day.  I figured that since we were engaged in this discussion I ought to study the whole story you told.  And, although I was a bit miffed about some of your rhetoric, I have to complement you on a very sweet analysis.

The lovely insight that the areas of certain shapes can all be calculated using the same basic formula (KxLxW) is one of those moments that I think only programmers and mathematicians can truly appreciate.

Overall, I thought your video provided a good example of the kinds of things a programmer must do when solving a constrained problem within a resource constrained environment.  Clearly (at least I think it should be clear) one would not prefer the KxLxW solution in a resource rich environment unless one was very sure that the business would not extend the problem to general shapes.  And, indeed, even if the problem remained constrained to shapes that allowed the KxLxW solution, the separation into the more traditional formulae would likely better match other programmer's expectations; and would not cause them to puzzle over, and re-validate, the relatively novel approach.  While this might deprive them of a delicious moment of insight, it would allow them to get on with their tasks without delay.

I don't know if you've read the work of Don Norman.  Long ago he wrote a book entitled _The Design of Everyday Things_.  It's well worth the read.  Within those pages he stated the following rule of thumb: _“If you think something is clever and sophisticated beware -- it is probably self-indulgence.”_  In a resource rich environment I fear the KxLxW solution could fall afoul of this rule.

> _This was written before Casey read my comment above._

---

**CASEY**: OK great, it sounds like we've gotten onto the same page about software categories. I'd like to give my characterization of the coding practice you're describing as it applies to something like LLVM, since that is a piece of software from the list I gave, and it happens to be open source so we know exactly how it is constructed (unlike, say, Visual Studio).

What I take you to be saying in the above paragraphs - and in your books and lectures - is that when programming a large piece of software like LLVM, the programmers do not need to be concerned about performance when they are programming. They should be primarily concerned with their own productivity. If it were, to use your previous example, a simple calendar app, then they would ideally __never__ think about performance. But in LLVM, since that is in the category where sometimes "nano/micro/milliseconds matter", then they will have to think about performance at some point. That point is when they find the program is running too slowly, whenever that occurs.

In LLVM, perhaps that is the first time someone tries to build a truly large program with it, like the Unreal Engine or Chrome or whatnot. When this happens, __then__ the assumption is that the performance problems will be in some isolated parts of the code (I believe you have referred to them in this discussions as "modules"), so __just those parts__ should now be rewritten to be performance-oriented.

That's how I interpret what you're saying so far, and also how I interpretted things you've said recently like "If my Clojure code is too slow, I can always drop down to Java", meaning that you could rewrite a portion of the code in Java if that part needed more performance.

Is that a fair characterization?

---

**BOB**: I'm one of those signatories of the Agile Manifesto who still believes in a bit of up-front architecture and design.  (Actually, I'm pretty sure they all do.  It was the latter zealots who thought it better to leap into code without any forethought).

In the case you mentioned I _hope_ I would have thought through the problem well enough to recognize where I might run in to performance problems and to therefore treat those modules with greater attention.  For example, I might have created a very attenuated version of the module and then subjected it to a torture test while profiling the behavior.  My concern, of course, would be the investment of a large amount of time and effort into an approach that ultimately failed to meet my customer's needs.  (Ask me why I worry about things like that ;-).

The bottom line, of course, is that Single Factor Analysis is _always_ suboptimal.  There is no _ONE TRUE WAY_.  (A point I tried to make several times in _Clean Code_.)

---

**CASEY**: I have a lot of questions I'd like to ask already, but your last answer segues into one of them best so I'll go with that one :) Already in this conversation you have talked about several critical performance implications in software architecture: the "nanosecond" concerns of an IDE parser, the division of "modules" into nano/micro/milli/second response time requirements, the suggestion that a programmer (in this case, you) might create "a very attenuated version of the module and then subjected it to a torture test while profiling the behavior" before writing a piece of software to ensure that the performance would be acceptable, and even the idea that you might have to pick different languages depending on the performance requirements (Clojure vs. Java vs. C, in your example).

And in summary, you've said, "Knowing these environments, and knowing which is best for the problem at hand, is something we all need to be proficient at".

Given all of that, I'd like to return to basically the original question: why were you _surprised_ that people, such as myself, associated "Clean Code" with effectively _the opposite_ of what you have written here with respect to performance? None of the things I just listed are given prominent placement in your teachings. I'm not suggesting that you can't find a sentence here or there in a book or blog post that gives a nod to performance, of course. But by volume, the things you're saying here get barely a nod.

As a concrete example of what I mean by that, here is an entire multi-hour, six-part lecture series you gave on "Clean Code" where not a single one of the things you mentioned here are discussed in the nine hour runtime:

https://www.youtube.com/playlist?list=PLmmYSbUCWJ4x1GO839azG_BBw8rkh-zOj

If performance concerns are as important as you suggest they are in this thread, why isn't there at least one hour out of nine dedicated to explaining to the audience the importance of things like learning about performance, planning ahead of time about what parts of the code may have performance implications, avoiding performance-harmful programming constructs in those situations, creating performance tests beforehand like the kind you described in the previous answer, etc.?

Another way to ask this question would be, is it possible you have taken for granted how important performance awareness actually is _even to you_, because perhaps you are habitually doing it when you program yourself, and thus you have not given it the prominence necessary to ensure your audiences - who often will know very little about performance - actually think about it at the right times and in the right ways? Especially if, as you said, "we all need to be proficient" at these things?

---

**BOB**: Frankly, I think that's a fair criticism.  And, as it happens, I taught a class yesterday in which I spent morme time talking about the performance costs, as well as the productivity benfits, of the disciplines and principles that I teach.  So thank you the nudge.

I don't think I used the word _surprised_.  Or if I did it was not in reference to the topic; it was more about the tone.  Enough said about that.

You asked me whether I had been taking the importance of performance for granted.  After some self-reflection I think that's likely.  I am not an expert in performance.  My expertise is in the pactices, disciplines, design principles, and architectural patterns that help software development teams efficiently build and maintain large and complex software systems.  And as every expert knows, and must fight against, expert hammers think everything looks like a nail.

You also asked me "why...".  To the extent that I have not answered that above, I'll simply turn the question around and point out that it is probably for the same reason that your video was solely focussed on the amplification of performance to the strident denigration of every other concern.  To a performance hammer, everything looks like a nail.  ;-)

That being said, I'm finding this conversation to be more beneficial than I had initially anticipated.  It has nudged a change in my perspective.  You should not expect that change to be enormous.  You should not expect me to make videos about how horrible Clean Code is ;-).  But if you watch the next 9 hour suite of videos I make, you'll probably see more than "barely a nod" towards performance issues.  I think you can expect two or three nods.  ;-)

Because, as you pointed out, I do consider performance issues to be important enough to anticipate and plan for.

---

**CASEY**: Honestly the nudge was most of what I hoped to accomplish here :)  And just to emphasize how important I think performance is today, I noticed while trying to edit this very file on github that if I type a paragraph that is too many lines long, it starts to get very slow and it's difficult to type!  It's only a few hundred characters, but there's so many layers of things piled up in the system that what should be instantaneous becomes unusably slow.  So one of the reasons I harp on performance so much is because it seems like software is getting unusably slow these days, even for simple tasks. In fact, just so you know I'm not making this up, here is a video where I record just how incredibly slow it was to type this paragraph:

https://www.youtube.com/watch?v=gPgm28zXNEE

And that's on a Zen2 chip, which is extraordinarily fast!  Whatever organizational forces (perhaps even cross-company in this case) make this sort of thing common would benefit greatly from hearing, for example, _exactly_ what you said earlier in this conversation.  There are tons of organizations that absolutely don't think about the "nano/micro/milli/second" breakdown, and they need to!  Just putting that thought in their heads - that they need to have institutional ability to recognize where performance problems will be _early_, before it's too late, and to have institutional players with the power to address those problems - would be a _major_ improvement in most development organizations.

So we could definitely end the conversation here. If you'd like to keep it going, the next thing to talk about would be the "strident denigration" you referred to. That would take us into architecture territory, not merely performance, but I'm happy to go there if you'd like. You're choice!

---

**BOB**: That video was hysterical.  I gotta ask what browser you were using.  I'm using Vivaldi (a Chrome fork) and it exhibits the same kind of lag. (Though not quite as bad as yours.) So I did a few experiments.  It turns out that the lag has nothing to do with the size of the file.  Rather, it has to do with the size of the _paragraph_.  The longer the paragraph the longer the lag.  Indeed, this paragraph is already unable to keep up with the 25cps repeat rate.  And the more I type in this paragraph the worse the lag gets.

Now why would that be?  First of all, I imagine that we are both typing into the same javascript code.  After all, nobody wants to use the tools written into the browser anymore ;-)  I mean, JavaScript is just _so much better_.  Secondly, I also imagine that the author of this code never anticipated that you and I would pack whole paragraphs into a single line.  (Note the _line_ numbers to the left.)  Even so, the lag becomes very apparent at the 25cps rate by about 200-300 characters.  So what could be going on?

Could it be that the programmer used a badly written data structure that grows by allocating a new memory block every time it grows, and then copies the data into the new block?  I rembmer the old Rouge Wave C++ Library had a growing string like that.  Boy, oh boy, could that get slow!  You can do the math.  That's O(n^2) if you ask me.

Of course that's much more of an algorithm problem than straight forward efficiency problem.  And, indeed, the algorithm is always the first place to look when something seems too slow.  But, your point is well taken.  The programmer here simply never anticipated the kind of use we are putting their code too; and it just doesn't deal well the unanticipated load.  They didn't think it all the way through.

Perhaps you and I should
hit return at the end of
our lines from now on. ;-)

                  10        20        30        40        50        60        70        80
         ....:....|....:....|....:....|....:....|....:....|....:....|....:....|....:....|
         Anyway, I am curious about the architectural issues you seem to have queued up
         for me. So, let us continue this conversation, while limiting our lines to 80
         characters -- you know, like a Holerith card.  That way, no matter what kind
         of chip this code executes on, it'll probably be able to keep up.  >B^D

---

**CASEY**: I couldn't resist, so I looked at the performance capture in Chrome and we now know who the culprit is :) It's the "emoji picker"! And your guess about the kind of problem was not far off.

I only glanced at the code, but the issue appears to be that every time you type a character, the "emoji picker" will scan backwards in the text to see if what you have typed is an emoji. You can see it "work" if you type, say, a colon followed by "bacon" or whatever. When you do that it will put up a little drop-down for emoji completions and show the cute little bacon icon. I can't recall ever using (or wanting) this feature, but, I guess it's there if we... uh... need it? Anyway, for long paragraphs this scanning process eventually gets slow enough to prevent responsive processing of keystrokes.

However, this newfound knowledge does provide us with a "work-around". Whenever we get to the point in a paragraph where the github text editor becomes prohibitively slow, we can just _pretend we are starting a new emoji_ and this solves the problem by preventing the picker from scanning as far backward. We can do this by just typing a colon, because that's the thing that normally starts emoji! For example, right now this paragraph is starting to be really sluggish. So I'm just going to put a colon right here : and voila! Now I'm back to normal typing speed, no worries. I could probably go a few more sentences like this just fine. Of course, I'd have to drop another colon at that point, but, it seems a small price to pay to see what I'm typing.

I would actually have to read the code carefully to find out _why_ the thing seems to scan backwards so far, when obviously the emoji strings can't ever be that long. What's the maximum length of an emoji name? Twenty characters? Thirty? It can't be much. But the fact that my work-around works seems to indicate I'm not wrong about the problem. So I guess they must just have coded something like "scan backwards until you see a colon" and left it at that.

Why it causes such a vicious slowdown is another interesting question. From the looks of the profile, I think what's happening is the emoji check just has to take enough time to be _longer_ than it takes you to type the next character. Once it gets up to a hundred milliseconds or so, I suspect what is happening is the next keyboard event comes in before it's done, so it just ends up woefully behind and it can't catch up until you stop typing :( But that's just a guess, I didn't actually investigate.

Anyway, I figured that might amuse you so I thought I'd share it with you before we continued our discussion!

---

**BOB**
    LOL:LOL:LOL:LOL

    Don't:you:just:love:being:a:programmer:and:diagnosing:interesting:problems:from:the:symptoms?
    To:do:that:well:you:have:to:_think_:like:a:programmer.::

    Long:ago:I:started:a:company:called:Object:Mantor:Inc.::My:partner:and:I:came:up:with:a:nice
    little:logo,:and:I:thought:it:would:be:fun:to:embroider:it:onto:a:few:shirts.::My:wife:had:an
    embroidery:machine:so:I:fired:up:her:yukky:PC:desktop,:fed:the:image:file:into:the:embroidery
    software,:and:out:came:the:file:that:the:machine:could:use:to:sew:the:pattern.

    That:file:was:a:little:crude,:so:I:decided:to:edit:it:to:fix:a:few:of:the:rough:edges.::But
    the:damned:thing:kept:crashing:when:I:selected:certain:parts:of:the:pattern.::Other:parts
    were:safe:and:the:software:behaved:fine:so:long:as:I:avoided:these:"hot":zones.::

    Now:why:would:it:do:that?::Ah,:well,:I:had:just:spend:a:couple:of:years:working:in:a:
    system:of:applications:that:made:heavy:use:of:computational:geometry.::One:of:the
    tricker:elements:to:manage:were:arbitrary:polygons.::You'd:like:to:think:that:polygons
    are:nicely:ordered:sets:of:points:that:describe:the:perimeter:of:a:shape.::In:reality
    of:course,:they:can:be:as:tangled:as:a:skein:of:yarn:that:a:kitten:has:been:playing:with.

    So:I:cranked:up:the:zoom:until:my:logo:was:the:size:of:a:football:field.::I:started:scanning
    the:periphery:of:the:polygons:that:the:image:processing:software:had:created.::Sure:enough,
    every:so:often:there:was:a:little:tiny:tangle:--:a:grouping:of:three:or:four:points,
    separated:by:no:more:than:a:fraction:of:a:millimeter,:that:tied:that:part:of:the:perimeter:in:
    a:little:knot.::

    I:manually:located:and:fixed:all:those:knots.::And:after:that:the:editing:program:stopped
    crashing.

    Oh,:I:could:go:on.::Perhaps,:one:day,:I'll:regale:you:with:my:first:use:of:the:Obama-care:
    website.::Now:there's:a:fun:tale.

>_I created this with vi and used_ `1,$s/ /:/g`
>_Because, I really am an old C hacker at heart._

---

**CASEY**: Now that we've discovered the secrets to efficient github text entry, we can move on to architecture :)

I suppose the best place to start would be to confirm some things from "Clean Code" that I agree with, and say the extent to which I agree with them. There are two main things.

First, I think we're in total agreement about basic code readability. Like you, I prefer code to be legible without comments, such that if you read a function from top to bottom, it more-or-less says in English what it does because the functions, variables, and types have been named so as to accurately portray their behavior. Obviously this isn't always _strictly_ possible, but that's the general goal. I don't love working with code that where all the variables are "a", "b", "c", "d", etc., or - and this is a real thing - code where all of the matrix operations in the entire program are written using two _global_ variables, A and B.

That said, I'm fine with single-letter variable names if they _reference_ something specific. For example, if the top of a function has a comment saying "this implements such-and-such a paper", and the variables are named to coincide with equations in the paper, I might prefer that. If the thing being implemented is some kind of complicated math, where I'm going to have to read the paper anyway to really know what is going on, having the variables line up with the paper can be a help in that case.

But otherwise, I like descriptive naming.

Second, I do like tests. Even in the video that precipitated this conversation, the code has some "self-testing", which is to say that it shows the difference off of a reference sum, just to try to catch any bad code changes that produce an incorrect area summation. Similarly, in my "Performance-Aware Programming" Course (where the video comes from), we recently did an 8086 disassembler. The way I developed it was I wrote a bunch of test ASM listings, then made a batch file that would assemble them with NASM, disassemble them with my disassembler, then reassemble the disassembly with NASM. I could then diff the final result with the original NASM-produced binary and know that my disassembly was accurate.

So I think we're roughly on the same page about tests. Where we might differ is to what extent tests "drive" the development process. I get the sense that your perspective is "write the tests first", or something close to that. I don't know that I would go that far. I tend to develop first, and as I find things where I think a test would help prevent regressions, I'll add the test then.

I think there are two reasons for this. One is that my background is game development, which is a highly interactive (and subjective) medium where many things are difficult to test in an automated way. So while we can (and do) test things like memory allocators or math libraries as necessary, testing broader things like "can the player jump between these two ledges" is not really feasible. We can do replay testing, and randomized input testing, but there are a fair number of things that just aren't feasible to "drive" via testing. This is less the case when looking at someething like, say, a file server, where you can very explicitly define what the inputs and outputs should be ahead of time. So I do think it varies by type of application.

The second reason I'm more judicious with tests is that the goal of tests is to save development time. If you end up writing tests that you don't use, or that don't find bugs, or that took longer to write and maintain than the number of bugs they prevented, that can be a lose. So I think there's a balance there, where not enough testing means that development takes longer because you have more bugs and spend more time hunting for "what broke" when you make changes, but too much testing means you spend a ton of time making tests that didn't actually bear fruit.

And that, too, is application-specific, or even component-specific, because bugs have varying costs. I may be worried or even paranoid about bugs that could affect file integrity in a file server, but relatively unconcerned about pixel-accuracy bugs in the renderer for the buttons that control that file server. So what I'm going to test, and how rigorously, seems like it has to be somewhat flexible if the goal is to minimize the total development time for a project.

But, like I said, I think we probably agree more on readability and testing than we disagree. I might just put more emphasis on weighing the costs when it comes to testing. Does that seem like an accurate summary?

---

**Bob**: I have a few quibbles that I'll explain below; but again I don't think our disagreements are huge.

>_Descriptive Names_

My rule for variable names is that the length of a variable's name should be proportional to the size of the scope that contains it.  The name `i` is perfectly valid within a one or two line scope.  Much larger than that, and you should likely use a small word, like `index`.  I'd make the same exception you do for well known formulae.  If you are writing the code for the quadratic formula, `a`, `b`, and `c` are the right names to use regardless of the number of lines in the scope.

BTW, my rule for functions is the opposite.  The name of a function (or a class) should be _inversely_ proportional to the size of the scope that contains it.  This is mostly for convenience (small names are easier to call) but also because at large scopes functions tend to be general, requiring no modifiers.  At smaller scopes we need adjectives or other modifiers to properly describe the function.  And, of course, exceptions apply as before.

>_Tests_

I am perhaps a bit more rigorous in the way I write tests.  I tend to write failing tests first, and then make them pass, in a very short cycle.  This helps me think through the problem.  It also gives me the perspective of writing the code _after_ something has used it.  It often forces me to decouple elements from each other so that they can be independenly tested.  Finally, it leaves me with a suite of tests that has very high coverage, and that I have seen both pass and fail.   And that means I trust that test suite.

But I also face the same dilemma you do.  I cannot practically write tests for things that I have to see in order to know that they work.  GUIs, Game behaviors, Hardware interactions, etc.  Writing tests for these things is pointless because I don't know the correct answer up front.  So I have to use my eyes in a very tight cycle making small changes to the code until the behavior matches my "feeling" for correctness.

After that I _might_ write a test if I think it's a behavior I want to anchor.  But often I leave that code untested.

Two examples:
  1. _Spacewar_.  On my gitub site you'll see a project named `Spacewar`.  It's fun little Star Trek game patterned after the kind of games we used to play in the 80s.  It's written in Clojure and is very interactive.  I tested the hell out of the computational, physics, and AI behaviors; but could not practically test the interactive graphics.  If you look at that code you can see that one side (the engine) is heavily tested, and the other side (the GUI) is not.
  2. _more-speech_.  Also on my github site you'll see a project called `more-speech`.  It is a browser and client for the `nostr` network.  It has a GUI component, and a lot of internal message processing.  The latter is tested, the former is tested less so.

Why do I use the test-first-discipline?  You named a bunch of reasons above, but neglected one that I consider to be the overriding impetus.  When I have a suite of tests that I trust, and that has high coverage, I can refactor that code without fear.  I can improve the design.  I can add new features.  I can make the code more performant.  And so long as all those tests continue to pass I can be certain, beyond reasonable doubt, that nothing has broken.  That ability makes me go _fast_.

I consider it to be the equivalent of double-entry bookkeeping for accounting.  I try to say everything twice and make sure the two statements agree.

---

**CASEY**: What you are describing is what I meant by "as I find things where I think a test would help prevent regressions, I'll add the test then." Maybe other people use the term differently, but "regression test" is the term I use to describe a test whose primary purpose is to detect bugs in modifications to a code path as I modify it. So what you described is exactly what I use a regression test for: allowing me to make more rapid or significant changes to a critical piece of code that I know might fail in subtle ways that would be hard to detect without extensive testing. It prevents the compliance from "regressing".

So I think we're in pretty broad agreement on testing. Maybe you spend a higher percentage of your coding time writing tests because we differ on the exact cost/benefit analysis there, but it doesn't sound like there's a huge gap either way. So perhaps we can move on to something we will disagree on, which is "classes vs. switch statements"? Of course those are just particular implementations of a more general divide, which is operand-primal vs. operation-primal architecture.

I don't want to misrepresent your opinion so I'll leave full categorization of the clean code methodology for your response. But to tee it up, from my outsider's perspective, I would say it tends to be "operand-primal". This is in line with traditional OOP ideas, and means the "rules of thumb" in the methodology tend to favor ease of adding new types for existing operations vs. ease of adding new operations for existing types. It also usually means that the programmer is encouraged to think about operations as things which exist subordinate to the types on which they operate, as opposed to the other way around.

Although I have worked in codebases that share that perspective, and have written that kind of code myself, I am unable to see the benefits of such an approach except in very rare circumstances which typically involve so little code that I don't tend to consider them as necessary considerations to the overall design. However, I am clearly in the minority, because OOP in general is obviously a very _popular_ approach today.

So I'm hoping that this part of the discussion will give me the opportunity to point out all the ways I think operand-primal design results in _both_ slower development time _and_ slower code runtime, and get a proponent's response. But before we do that, perhaps you could respond to my categorization first, in case you'd like to either disagree with it or elaborate more?

---

**Bob**:
This is so much fun.  It is a pleasure to discuss these matters in such a professional and civil manner.

>_Tests: afterword_

It seems the difference between us can be stated as follows:
 * I write tests unless there is a good reason not to.
 * You write tests when there is a good reason to.

I consider the former to be a _discipline_.  As a pilot I follow checklists unless there is a good reason not to.  I fly in good weather unless there is a good reason not to.  I submit myself to air traffic control, even when legally unecessary, unless there is a good reason not to.

>_Categorization: The Primality of Operand vs Operation_

I like the categorization.  Indeed I wrote a whole chapter in _Clean Code_ on this topic, though I didn't use your names (Chapter 6: Objects and Data Structures).  OO (Operand Primal) is a great way to add types to existing functions without changing those functions.  Procedural (Operation Primal) is a great way to add new functions to existing types without changing those types.

The question, of course, is which of those two is the most efficient?

 * From the point of view of counting nanoseconds, OO is less efficient.  You made this point in your video.  However, the cost is relatively small if the functionality being deployed is relatively large.  The _shape_ example, used in your video, is one of those cases where the deployed functionality is small.  On the other hand, if you are deploying a particular algorith for calculating the pay of an employee, the cost of the polymorphic dispatch pales in comparison to the cost of the deployed algorithm.
 * From the point of view of programmer effort, OO may or may not be less efficient.  It _could_ be less efficient if you knew you had all the types.  In that case switch statements make it easier to add new functions to those types.  You can keep the code organized by function rather than type.  And that helps at a certain level of cognition.  All else being equal, programs are about functions.  Types are artificial.  And so organizing by function is often more intuitive.  But all else is seldom equal...
 * From the point of view of flexibiliby and on-going maintenance, OO can be massively more efficient.  If I have organized my code such that types define the basic functionality, and variations in that functionality can be relegated to subtypes, then adding new variations to existing functionality is very easy and requires a minimum of modification to existing functions and types.  I am not forced to find all the switch statements that deploy functions over types and modify all those modules.  Rather I can create a new subtype that has all the variations gathered within it.  I add that new module to the existing system, without having to modify many other modules.

That last bullet was a very brief and imperfect explanation of how OO helps programmers conform to _The Open-Closed Principle_ (OCP). i.e. it is better to add new modules than it is to change existing modules.

Are there cases where using OO does not help conform to the OCP?  Certainly.  Again, if you know all the types, and you expect variation to be new basic functionality operating over all those types, then dynamic polymorphism (OO) works _against_ the OCP and switch statements work for it.

That would be the bottom line if it weren't for one other factor:  Dependencies.

The cases of switch statements create an oubound network of dependencies towards lower level modules.  Each case may call out to those other modules, making the fan-out of the switch statement very high.  Any change to one of those lower level modules can force the switch statement, and all higher level modules that depend upon that switch statement, to be recompiled and redeployed.  That can be a very large cost.

On the other hand, if one uses dynamic polymorphism (OO) instead of a switch statement, then those compile time dependencies are _inverted_.  The lower level modules become subtypes that depend upon the higher level base type.  The source code dependencies then point in the opposite direction of the flow of control.  This is _Dependency Inversion_, and it prevents changes at to low level modules from forcing a wave of recompilation and redeployment from sweeping through the system towards higher level modules.

This notion of inverting dependencies is the foundation of my architectural argument.  But, perhaps, this is a good place for me to stop and get your reaction so far.

---

**CASEY**: That's actually great, because I don't see how that argument works, so starting there will be very illuminating.

I don't disagree with the _term_ "dependency inversion", because in a sense you _are_ "inverting" something (to me, it is analogous to swapping the dimensions of an array). But from my perspective you are not creating a benefit, you are simply making the same trade again, where you favor _operand_ addition at the expense of _operation_ addition.

Specifically, suppose you have n types each supporting m operations. Any system design supporting this will therefore have O(nm) "things" in it, and the design question is how do you want to group them. We'll leave out the fact that sometimes this can be compressed (or "made more sparse") for now. Switches and classes do compress in different ways and I would like to talk about that later, but to avoid talking about too many things at once, let's first talk about the "worst case" where every operation does something different for every type.

Dynamic polymorphism is the design where you have n types containing m member functions. It ensures that if you would like to move from n types to n+1 types, then you are going to create a new type and implement m new member functions in it. This means you only have to "recompile and redeploy", as you put it, that one file, or something on the order of that.

By contrast, the switch statement design is where you have m functions each containing n cases. If we want to add a new type, we have to go through each of those m functions and add one new case each. This requires recompiling and redeploying m files instead of 1 file. I _assume_ this is the specific benefit to which you were referring, and I don't disagree with that premise.

However, suppose that you instead want to add a new _operation_. Now I am leaving n the same and moving from m operations to m+1 operations. In your model, you now have to go through _every single class_ and add that new operation, which forces you to recompile and redeploy n files, one for every class.

By contrast, the switch statement version is already separated by operation, so you do not need to touch any of the existing files (or modules, etc., whatever the storage medium is we are imagining here). You simply create one new file for this new operation, implement the n cases, compile it, and deploy it. It is the exact mirror of the type case.

So to me, there is no "win" here in the abstract. You are merely choosing _which_ programmer behavior you will make hard, and which you will make easy. By choosing dynamic dispatch, you make adding operations require more recompilation/redeployment, because it is the "inner multiple" in that grouping. By choosing static dispatch, you make adding types required more recompilation/redeployment, because _that_ is the "inner multiple" in that grouping.

Note that in both cases you are always adding the same number of _things_ (member functions or cases). The only question is _how spread out_ they were. Both cases have one type of change where the code is clustered together, and one type of change where the code is scattered, and they are exactly "inverted" in which one it is. But neither gets a win, because they are both equally good or equally bad when you consider both types of additions (types and operations).

Clearly you view this differently, because you said, "That would be the bottom line if it weren't for one other factor:  Dependencies." where "bottom line" was already the idea that you're just trading off between ease of adding types vs. ease of adding operations. So I assume that means you think this "dependency inversion" does something _more_ than that "bottom line" already did, whereas I see it as being exactly the same bottom line as before: it's just the exact same tradeoff again.

Where does your analysis differ?

---

**BOB**: Casey, we are on exactly the same page with one big exception: _Dependency Inversion_.  But first let me restate.

Yes, every program composed of `o` operations and `t` types has a complexity of `o`x`t`.
 * If we use OO we can increase `t` with minimum disruption to the source code; but increasing `o` disrupts many source code modules.
 * If we use switch statements we can increase `o` with minimum disruption to the source code; but increasing `t` disrupts many source code modules.

So I believe we understand each other so far.

However, notice that the issue, in both cases, is _source code management_.  It has nothing to do with the runtime execution.  So let's split this problem into two issues: Run Time and Source Code.

>_Run Time_

I propose a hypothetical compiler that produces identical binary code irrespective of whether the input is operand or operation primal.  If you give this compiler OO source code S1, it will produce binary output B1.  If you give this compiler procedural source code S2 it will produce binary output B2.  So long as the behavior of the two programs is identical, then B1 and B2 are also, byte for byte, identical.

It should be clear that a compiler like this is possible since switch statements can be (and often are) compiled as jump tables; and polymorphic dispatches can be (and often are) compiled as jump tables.

Yes, there are complicating issues; but let's ignore them and agree that such a compiler is at least feasible.

Thus, since they can be compiled down into identical binary code, there is no necessary Run Time difference between the OO and Procedural styles.

>_Source Code_

As we have already agreed the swapping of `o` and `t` makes very little difference to the problem of source file management.  Individual cases may favor one style or another.  For example if we have stable operations but variable types then OO might be favored.  Or if we have stable types but variable operations then a Procedural style may be favored.

So it looks like, all else being equal, the two styles have the same capabilities.  There is no Run Time difference, and the Source Code management issues are just mirrors of each other.

>_Dependency Inversion_

But all else is not equal.  There is a radical difference in the source file dependency structures created by the two styles.

 * **Operation Primal**: In the procedural style, in which switch (if/else) statements deploy operations over types, the source code dependencies (#include statements in C/C++) follow the Run Time dependencies.  High level modules that call lower level modules must #include those lower level modules.  So the #include chains form a Directed Acyclic Graph (DAG) in which every edge is directed from a higher level module to a lower level module and follows precisely the direction of the function calls. (Ignoring the .h .c divide which we can discuss at length later.)
 * **Operand Primal**: In the OO style, in which dynamic polymorphism is used to deploy types over operations, higher level modules are _independent_ of lower level modules.  The #include statements point in the opposite direction.  Subtypes depend upon their base types.  Thus the source code dependencies point _against_ the runtime calls, _against_ the flow of control.  The edges of the #include DAG are directed from lower level modules towards higher level modules.

The implications of this difference are:
 * When using the procedural style high level policies depend, at compile time, upon low level details.  If those low level details change for any reason, the high level policies must be recompiled and redeployed even though they have not changed in their intent.
 * When using the OO style high level policies are independent of low level details.  If those details change in implementation, the high level modules remain unaffected.  They do not need to be recompiled or redeployed.

>_Example_

In the 1950s and 1960s we would write code that directly used the IO devices of the day.  These devices were things like card readers and punches, paper tape readers and punches, teletypes, printers, and magnetic tape drives.  If someone asked us to write a payroll application that took in employee data as punched cards and wrote paychecks on the line printer then our code would do _exactly_ that.  It would explicitly read from the card reader, and explicitly write to the line printer.  We used the _Operation Primal_, procedural, style.

But then our customers would come to us one day and ask us to read the employee data from magnetic tape, and output the paycheck data to another magnetic tape.  To them, this was a low leve detail.  The data was still the same, it was just on a different medium.  But to us it was a complete architectural rewrite.  Magnetic tape drives are very different from card readers and line printers. And so the change was very expensive, and our clients could not understand why.

To defend against this we invented _Device Independence_.  We created an abstraction called a _File_ and gave it standard operations such as `open`, `close`, `read`, `write`, and `seek`.  (The five standard functions of the UNIX IO driver).  We categoried our IO devices as _types_.  We used dynamic polymorphism (jump tables) to implement the dispatching.  By creating a stable set of operations, we forced virtually all of the variation into the types -- the different IO drivers.

From then on, we could write our applications without knowing, or caring what IO devices we were using.  And life become a _lot_ simpler for the programmer at large, and a lot less expensive for clients who made "small" changes to the media they wanted to use.  Those changes were made in the job control directives without forcing any source code change, recompilation, or redeployment of the application.

>_But wait..._

You could easily make the point that while the OO style protects high level policy from changes to low level detail, it also makes low level detail _vulnerable_ to changes in high level policy.  That's true.  And by the same token, the procedural style certainly makes high level policy vulnerable to changes in low level details, but protects those low level details from changes to the high level policies.

So the question then becomes one of architectural philosophy: Which of those two is more likely to change?  Does policy change more frequently than detail?  Or does detail change more frequently than policy?

---

**CASEY**: Before getting into the details here, what do you mean by "I propose a hypothetical compiler that produces identical binary code irrespective of whether the input is operand or operation primal"? Do you mean a hypothetical _language_, where it doesn't matter which you do? Because that doesn't seem very useful, since I could just say "I propose a hypothetical language where the dependency cost is always the same" or something, and then we have nothing to discuss because we are both talking about fictitious systems. It doesn't really address how we're supposed to write actual code, which is what we're talking about here.

So are you suggesting that you think that current C++/JAVA/etc. compilers _do_ exhibit this property? I just want to unpack this so I can understand what you mean by the "run time" section above.

---

**Bob**: Let's make it a C++ compiler.  You pass this compiler a C++ program written with switch statements, or the equivalent program written with base classes and subtypes, and it generates the exact same binary code.  Let us further stipulate that the binary code it generates is as efficient as possible.  Such a compiler ought to be possible since switch statements and dynamic polymorphism are both often implemented as jump tables.

---

**CASEY**: I would again say that this is not a useful hypothetical for a discussion about real-world coding practices. In production, if you are compiling C++, the vast majority of programmers will be using CLANG, GCC, or MSVC. None of those do what you're describing. While virtual methods are always implemented with a vtable pointer in the object, switch statements can be optimized in a wide variety of ways, all of which are more optimal than an indirected function pointer (which is what all virtual calls are). The only time you will get similar optimization from a virtual function call is if it's not actually virtual because you have an explicit pointer to a derived type, and the method has been marked "final" for that type. And that is not a comparable case, because you wouldn't need a switch statement to handle that case either.

---

**Bob**: My apologies for the distraction.  It was not my goal to start a debate about the possible existence of a hypothetical compiler.  My goal was only to set the performance aspects of the two different schemes aside so that we could focus on the architectural issues that you have proposed that we discuss.

So, I hereby withdraw the hypothetical compiler from further consideration and recommend instead that we focus on the architectural effects of operand-primal vs operation-primal approaches.

---

**CASEY**: Well, we do still need to consider the run-time part of your claim though. If I can summarize the claim, it was:

* Run-time: No difference
* Source code: No difference
* Dependency graph: Favors operand-primal

If you are withdrawing the hypothetical compiler from consideration, is the claim now:

* Run-time: Favors operation-primal
* Source code: No difference
* Dependency graph: Favors operand-primal

or is it something else?

---

**Bob**: So stipulated.  Please proceed.

---

**CASEY**: OK, proceeding from there, I'd like to now focus on whether or not there is actually a benefit from the dependency inversion that you're describing. This will probably take a few back-and-forths, but, I'll start with this:

You gave the example of low-level IO routines, like reading bytes from a tape drive vs. a magnetic drive. I, of course, agree completely that there may be many circumstances where you would like to write a lot of code in terms of some series of operations like "open", "read", "close", and not care exactly what is being opened, read, or closed. But that goal can be achieved with either architecture.

In the case of a class hierarchy, the user is passing around a base class pointer, and they call ->read() on it. In the case of a union (or any other non-hierarchy technique), you are passing around a pointer to that, and you call read() and pass the pointer as the argument. In neither case does the user have to know (or care) about what that call does.

So I just want to start with some understanding of why you chose that example. It doesn't seem to me to differentiate between the two methods at all. From the user's perspective, the only difference they will experience is syntactic - eg., they type ptr->read(...) instead of read->ptr(...). But why do they care about that? What are the specific benefits they are getting from this?

You mentioned "recompilation and redeployment". By "recompilation" I assume you mean that if you are calling through a blind table, then you do not need to recompile the calling code. By "redeployment", I'm not sure exactly what you are referring to, but I'm imagining a scenario where you would like to ship, say, several DLLs, and only send one new DLL instead of the entire set of DLLs?

So perhaps you could elaborate on those, and then add any other benefits you think are flowing from this, so I can get the full picture?

---

**Bob**: To begin with it appears that we agree that there is a benefit in organizing the source code such that high level policy does not depend upon low level detail. I think we also agree that this benefit is _human_ as opposed to mechanical.  By that I mean that the computer doesn't care whether high level source code is independent of low level source code.  Only the humans care about that.

I infer this agreement from your statement:
>_"I, of course, agree completely that there may be many circumstances where you would like to write a lot of code in terms of some series of operations like "open", "read", "close", and not care exactly what is being opened, read, or closed."_

It's probably worthwhile listing some of the reasons _why_ this separation is important to humans.

 * It allows us to use, modify, and replace low level details (e.g. IO devices) without impacting the source code of the high level policy.
 * It allows us to compile high level components without needing the compiler to read the low level components.  This protects the high level components from changes to those low level components.
 * It allows us to set up a module hierarchy that has no dependency cycles. (No need for `#ifndefs` in the header files. ;-)  Which keeps the order of compiles and deployments deterministic.
 * It allows us to break up our deployments between high level and low level components.  So, for example, we could have a high level DLL/JAR/EPROM and a low level DLL/JAR/EPROM.  (I mention the EPROM because I used precisely this kind of separation in the early 80s with embedded hardware that had to be maintained in the field.  Shipping one 1K EPROM was a lot better than shipping all 32 ;-)
 * It allows us to organize our source code by level, isolating higher level functionalities from lower level functionalities. At every level we eschew the lower level concerns, relegating them to source code modules that the current level does not depend upon.  This creates a hierarchy of concerns that is, in human terms, intuitive and easy to navigate.

You then said that this kind of isolation is possible with "either architecture". For the sake of this discussion I will accept the use of the term "architecture" to mean one of the two styles we are debating: operation primal vs operand primal.

So, are the bullet points above possible with both architectures? Let's set up an experiment.

Here is the high level policy of my payroll system written in Java(ish).  I would like this source code module to be independent of all low level details.

    public class Payroll {
      public void doPayroll(Date payDate) {
        for (Employee e : DB.getAllEmployees()) {
          if (isPayDay(e, payDate)) {
            Paycheck check = calculatePaycheck(e, payDate);
            pay(e, check);
          }
        }
      }
    }

This module is very high level.  It states the highest level algorithm for paying my employees.  It does not mention the fact that some employees are salaried, others are hourly, and others are commissioned.  It does not mention the fact that hourly employees are paid weekly, commissioned employees are paid twice a month, and that salaried employees are paid monthly.  It does not mention the fact that hourly employees are paid time and a half for overtime, or that commissioned employees are paid a base salary plus a commission on their sales reciepts.  It does not mention the fact that some employees want their paychecks directly deposited, while others want them mailed to their home, while still others want to pick up their pay from the paymaster.

Using the operand primal style I can very easily ensure that the above module remains independent of all those details and that no `import` statement in this module mentions, directly or transitively, any module that implements those details.  I can compile the above module into a JAR file that could remain unchanged while the JAR files that contain all the details thrash endlessly under a barrage of requirements changes.

Can I do that with the operation primal style?  You mentioned passing around a `union`.  I presume my module would have to `import` that `union`.  So how can I define that `union` such that it makes no mention of any of the afore mentioned details. I suppose we could use opaque (`void*`) pointers, and take advantage of the late binding of the linker to push the problem down one level.  But I'd like to see what you have in mind.

You mentioned that the difference in my _Device Independence_ example was purely syntactic -- a matter of `ptr->read(...)` instead of `read->ptr(...)` by which I think you meant `ptr->read(...) vs read(ptr, ...)`  But I think we disagree on that.  In my example the dynamic polymorphism of the _File_ abstraction was pushed across the boundary of the operating system.  The high level interface _File_ was usable by all applications.  The low level implementations were device drivers linked into the OS.

This meant that I could write an application using the _File_ abstraction.  I could compile, link, and deploy it.  And it would still have no dependency upon, nor reference to, the low level devices it was intended to control.  That linkage occurred at runtime when the pointers to the `open/close/read/write/seek` functions were loaded into the _File_ vtable.  Thus, new IO devices could be added to the OS, and my application could control them, without the need to recompile, relink, and redeploy it.  It seems to me that might be tricky with the `union` approach.

You asked me what I meant by recompilation and redeployment.  Recompilation is the act of rebuilding the application.  This can be as simple as `cc app` or as complex as some gigantic `makefile` might (um) make it.  The development team might have to build the app for N different plaftorms, and run all the tests for all those patforms.  The build procedure can take milliseconds, or it can take hours, depending on the complexity of the application.

Deployment has a similar range of complexities.  It might simply mean `cp app /usr/bin` or it might require a massive deployment of executables around a network of servers. It might require a very carefully tuned and timed startup procedure.  Depoyment can be a matter of milliseconds or days depending upon the complexity of the system.

I hope this helps to answer your questions; because I am eager to get on with the _architecture_ part of our discussion.

---

**CASEY**: It sounds like you're assuming that the programmer has to expose certain things in the "union" case that they do not actually have to expose. In practice it is no different from the hierarchy case in terms of how much _data_ you wish to expose. In the hierarchy case, for example, you _could_ put a bunch of data in the base class, which exposes it to the user. It's a _choice_ not to do that, which you make when you want to insulate the user from changes to that data.

The same is true in the union case (or any other non-hierarchy method - there are others, which are often better than the union method depending on the circumstances). Here is what the user would see in, say, a typical H file of a non-hierarchy library:

```
struct file;
file *Open(...);
void Read(file *File, ...);
void Close(file *File);
```

There is no need for any of the things that you're suggesting to be exposed. The only reason you would expose them is if you would like to get the performance increase that comes from allowing the compiler to optimize across the call. This choice is up to the maintainer, and they can choose to get the extra speed if they know that isolation is not beneficial, or they can choose not to get the extra speed if isolation is more important.

This design doesn't require exposing anything you didn't _want_ to expose. Furthermore, unlike the hierarchy case, it does not impose any constraints on the designs of the datatypes. Contrary to what you suggested, it has _less_ constraints across the interface boundary, not more.

Why? Because in the hierarchy case, you are moving the dispatch code from inside the library to outside the library. At compile time, the user of the library code is having the vtable dispatch code _compiled in_ to their program. Instead of just calling a function address and passing a pointer they know nothing about, which is what happens in the non-hierarchy style, they must _know_ how to translate that pointer into a function address by reaching into the operand and pulling something out (in this case a pointer to a vtable).

Another way to say this is, you _are_ actually pushing an implementation detail across the boundary with the hierarchy design: you're mandating how operand pointers turn into function pointers, which is a _constraint_ on the layout of the underlying operand. It _must_ have that additional data stored in _precisely_ one way, and cannot be changed on either side of the boundary without recompiling the other side. This is why, for example, C++ vtable layouts had to be standardized: because otherwise there would be no way for two programs compiled with different compilers to call each other's virtual methods.

Just to further illustrate this point using an extreme example, imagine if the library author decided they no longer want to use pointers for some internal reason. In the non-hierarchy case, this "just works". A pointer is just a number, and since the user of the library does not do anything with that number except pass it back to other library functions, it can be _anything_. It could just be an index into some table the library maintains. This is free to do and trivial to implement in the non-hierarchy case because the user's code never manipulates the pointer.

Not so in the hierarchy case. In the hierarchy case, the user looks into the pointer all the time, to pull out the vtable address. There is no way to hand the user something else, like an index, because the vtable scheme has to have been agreed upon systemically by unrelated _companies_, not just teams (in this case, the compiler vendors). So it simply can't do anything like this.

If anything, I see your arguments as actually being in favor of the non-hierarchy method, even though you seem to be saying they favor the hierarchy method. But I don't see how. The non-hierarchy method is a many ways a _superset_. It has more range both in terms of isolation from changes (no vtable scheme exposed), and in terms of performance because at the other end of the spectrum, the library author can choose to provide the entire source and give the user the option of compiling everything together for optimization purposes.

So I am not seeing where the benefits of hierarchy-based design come in here. They seem strictly less flexible at both ends of the "compilation and deployment" spectrum. And this "worse at both ends" aspect of the design is a major reason I think the hierarchy case should be abandoned as a general architecture: the programmer never seems to be better off when using it. If they wanted isolation, they got _less_ isolation with a hierarchy than with a blind pointer. If they wanted performance, they got _less_ performance because optimization can't occur across base class virtual calls no matter how much of the source code you expose to the user. It's _always_ worse than the alternative.

And the other important point here is that it is _continuous_ for the library author to make this decision in the non-hierarchy case. They can write a library that exposees only blind pointers, and then later decide to expose some or all of what those pointers point to, and the routines that operate on them, if/when they find that performance becomes a priority for a user. Even better, they can decide to give the user that choice if they wish: because the code itself doesn't change at all, only the amount of it exposed to the user changes, the user could choose how much exposure they want. If the user doesn't need the extra performance, they build without using the exposed source. If they do, they build with it, knowing they will have to recompile their code if the structures change in a future revision.

Nothing like this is possible in the hierarchy case: the library vendor would have to manually rewrite everything to eliminate the virtual call hierarchy first in order to make a change like this, which requires substantial effort, perhaps even a prohibitive effort in a large library. This is because the choice to base the design around virtual calls prevents compiler optimization across calls _everywhere_, even internal to the library, and there is no easy fix.

So hopefully this explains the confusion?

---

**Bob**: There's a lot to unpack in there.

 * You and I are apparently using a different definition of the term *union*.  I was assuming you meant the traditional C `union` in which the data members share the same memory space.
 * The code you presented is, in fact, the opaque pointer, linker bound solution I was alluding to above.  I said `void*` and you used `file*` with a forward declaration of the `struct file;`  So on that account I think we agree.
 * You repeatedly used the term _hierarchy_.  I presume you were referring to _inheritance_ hierarchies.  However, using such hierarchies is not my position in this debate.  There are times when a hierarchy can be useful; but in general I prefer the interface/implementation approach.  In C++ that's a pure abstract class with no implementation (the interface), and a concrete class that implements that interface.  I suppose you could call that a two level hierarchy if you like; but I want to be clear that I'm not a big proponent of deep inheritance hierarchies.

I agree that if you use the C style .c/.h split and ensure that the only thing that appears in the .h files are _forward declarations_ without any definitions, then you can hide the implementation quite nicely.  That's because the old C style .c/.h split _is_ dependency inversion.  (indeed, it is a two level hierarchy) The .c file contains the low level implemenation, and it depends upon the high level interface in the .h file.

>_As an aside, this pleasant .c/.h split was corrupted by C++.  Stroustrup needed to know the size of objects in order to implement the `new` keyword.  So he forced member variable definitions into the .h files -- thus destroying encapsulation, forcing the horrible public/protected/private hack upon us, and making dependency inversion much more difficult.  (e.g. the PIMPL pattern.)_

I also agree that the dependency inversion of the .c/.h split provides the protection I desire for compilation and deployment.  That protection comes from the fact that the linker does _late binding_ of the external references to the external definitions.  It's that late binding that allows the individual source files to be compiled independently.  Indeed, nowadays we use the old trick from the '60s of a linking loader to do all the linking at load time.  But that's another story.

So far we are on the same page with the minor exception that I'm still not sure what you meant by `union`.  But let's let that pass for the time being.

Perhaps this is a good time to reassess the differences and similarities in our position.

 * You are defending the .c/.h approach of inverting dependencies and using link-time late binding to protect against recompilation and redeployment.
 * I am defending the interface/implementation appraoch of inverting dependencies (which is essentially the .c/.h approach with a different syntax), and using run-time late binding (dynamic polymorphism), to protect against recompilation and deployment.  (As an aside, I actually use, and recommend, both approaches.  There are times when link-time binding is adequate and other times when runtime-binding is invaluable).

Oddly, we seem to have replaced the operand/operation concept with the kind of late-binding we prefer.  It should be clear that the both operands and operations can be bound after compile time since both are really just functions that take arguments.  So the real issue between us is not operation vs operand, it is link-time binding vs. run-time binding.

Your next assertion was that there was a difference in the amount of information that had to cross the boundary between the interface and the implementation.  Your argument was that both sides had to agree on the format of the vtable.  That's certainly true in C++.  Stroustrup created an indexed table of pointers to functions, and both sides must agree on the indeces for the method names _at compile time_.  I completely agree that programmers must ensure that the compilers used for the interface are compatible with the compilers used for the implementation.

Other language systems use different approaches that don't require that index agreement at compile time.  Some use string matching at runtime.  Others could negotiate the indeces at link time.  There are a multitude of strategies to mitigate this.  However, the indeces of vtables is just the tip of a much larger information iceberg.

If different languages, or even different vendors of the same language, are used across the interface/implementation boundary then they must agree upon a whole plethora of formats.  They must agree on endians, on the order of arguments and local variables within the stack frame, the size of data types, and the format of any try/catch markers on the stack.  The coordination of vtable indeces pales in comparison to all the other stuff they have to agree upon.

Even if we stipulate that the language systems are identical across that boundary, the interface and implementation must agree on the names and signatures of the functions.  So, given these facts, I'm not disposed to put a lot of weight on your argument about the vtable format.

Your argument that in the .c/.h case the user has the opportunity to gain optimizations by _compiling_ the modules together rather than using the late binding of the linker certainly has merit.  If you abadon _all_ late binding and give the compiler access to _all_ the source code, then the compiler can do quite a bit of magic.

However, this puts me in the position of invoking the hypothetical compiler that can do all that magic if it sees all the source code that uses dynamic dispatch.  Dynamic dispatch certainly adds complexity to the problem; but there's nothing impossible about it.  And, indeed, there are language systems that try to make such improvements both at compile and runtime.

But given the current state of most compiler I think your argument has enough merit to warrant half-points.  ;-)

Now let us say that we _want_ to protect our source code modules from recompilation and redeployment when low level details change; and that we are therefore going to use some kind of dependency inversion and late binding to provide that protection.  That means we are not going to let the compiler to see _everything_ but are intentionally _hiding_ information from the compiler thus preventing it from doing all the magic optimization it could do.

What, in this case, is the advantage of run-time binding over link-time binding.  Why would we want the vtables, or the various other forms of dynamic dispatch that our languages offer?

Sometimes we don't.  That's why C++ gave us the `virtual` keyword.  That's why java has `static` functions.  Sometimes we just bloody don't want that dynamic dispatch sitting between the caller and the callee.

But sometimes we do.  For exmaple: _Plugins_.

Visual Studio provides dynamic dispatch of functionality within their product so that vendors like _Jetbrains_ can build plugins like _Resharper_.  The use of linking loaders means that we can link these kinds of plugins into our applications at runtime, and the dynamic dispatch means that the application can be entirely ignorant of the source code of the plugins.

Related to plugins are frameworks like Rails.  Such frameworks could be statically linked if necessary; but otherwise convey similar advantages.  The framework need know nothing at all about the source code of the application that uses it.

In both cases there is a profound asymmetry based on runtime bound dependency inversion.  The high level side is ignorant of the lower level side and _invulnerable_ to it's changes.  The low level side is dependent upon the high level side, and must react to changes made to the high level.

This vulnerability assymetry means that Microsoft is invulnerable to Jetbrains, but Jetbrains must react every time Microsoft makes a change to VS.

This same kind of vulnerability assymetry can be advantageous between, or even within, development teams.  We don't want the business rules to be vulnerable to the GUI or DB.  Indeed, we'd like the GUI and DB to be plugins to the business rules.

But I think that's enough for this go-around.  So let me summarize where I think we are.

 * We both agree that dependency inversion and late binding are valuable for protecting systems against recompilation and redeployment.
 * We both agree that link-time late binding can be used to an advantage.
 * We both agree that it is sometimes necessary to abandon late binding so that the compiler can optimize.
 * And I HOPE we both agree that there is a time and place for dynamic late binding; such as plugins and frameworks.
 * And lastly, I hope we both agree that all these issues are human issues.  The computer does not care.

---

**CASEY**: Regarding "You and I are apparently using a different definition of the term *union*.  I was assuming you meant the traditional C `union` in which the data members share the same memory space.", you were assuming correctly: the "file \*" I'm talking about would be exactly that kind of union. In other words, inside the library, it is:

```
enum file_type
{
  // Types of files go here
};
struct file
{
  file_type Type;
  union
  {
    // Data storage types corresponding to file_type go here
  };
};
```

My point in the example is to demonstrate that you are able to either hide this or not depending on your preference. If you would like the user to be able to get optimization by compiling "through" the union, you put this in the H file. If you don't, you don't. It's your choice.

Furthermore, you only need that union if the particular usage of the library _actually is_ polymorphic, which it might not be. For example, if the particular system you're running on only has one type of drive (to continue the example), then the version of the library for that platform doesn't need the union anymore, meaning that the linking will produce a direct link to the correct function. Contrast this with the inheritance hierarchy version, which _does not_ do that: it will still compile to an indirected vtable dispatch, which is less efficient for no reason. Of course, this is not an efficiency I actually care about much, because if we are already calling across a library boundary then it is unlikely to actually matter that much. But I'm just pointing it out as yet another point in favor of not using virtual functions.

My point here relates back to my original point, which is that discriminated unions (C++ also has a janky std library version of these called "variants") seem to always be the better rule of thumb. If you use an inheritance hierarchy as your preferred mechanism for polymorphism, you are effectively "stuck". You cannot actually decide to change that decision later, because the code has already been written in such a way as to prevent optimization "through the union". On the other hand, if you use discriminated unions like this as your default case, then you can change your mind whenever you want. Start off with it hidden, if you want to keep compilation dependencies down. Move the structs and functions up into the .h file later later if you want to increase optimization.

Regarding "Oddly, we seem to have replaced the operand/operation concept with the kind of late-binding we prefer", not really. I don't actually prefer this style of "guard against people using the code". Nor do I think it's how you would normally write things unless you actually expect there to be a great amount of late-addition of types to the system. I agree broadly, for example, with the "Data-oriented Design" people: most of the time, you would be better off if code always knew what type it was dealing with. If you have circles, rectangles, and triangles, then you have three separate arrays, one for each type, so that there never _is_ a dispatch.

So I think the idea of structuring code around swappable types is generally wrong. I think it should only be used when you are specifically designing the boundary of a library or plug-in system, and rarely anywhere else. I really am "operation first", never "operand first". The point of the `file` example was to point out that it is _also_ trivial to take union-style code and hide its implementation details if you want to. It is incorrect to suggest that it is "worse" than inheritance hierarchies at this somehow. I _disagree_ than inheritance hierarchies have a "dependency inversion" advantage, and my understanding of your argument was that that was what you were suggesting.

I'm also not sure how "And lastly, I hope we both agree that all these issues are human issues.  The computer does not care." squares with "We both agree that it is sometimes necessary to abandon late binding so that the compiler can optimize."  If the computer didn't care, we wouldn't need to worry about optimization. But the computer definitely _does_ care, because it runs some kinds of code quickly and other kinds slowly.

So if I had to restate the summary, I would say:

* We both agree that _complete_ hiding of implementation details can be useful, and that people should know how to do it.
* We disagree on how often they should be hidden; you think the answer is "most of the time", I think the answer is "only in specific circumstances"
* We disagree on how important the computer is. I think we need to think about the computer most of the time, whereas you do not.

And by "complete hiding" I mean the kind where the user literally cannot see the details - eg., not just a "private" section of something they can see but not use, but rather their code is compiled without any knowledge of the internals at all.

---

**Bob**:  It seems we have come a long way and wound up at an agreement on just about everything other than individual preference; which is likely driven by the different environments we live in.

Thank you for the `union` clarification.  I understand it now.

I'll quibble with you a bit on the distinction between operand vs. operation, but I don't think the quibble is particularly important.  In the end it's all `f(x)` regardless of how you spell it.

As for the "human" issue.  Performance is a human issue.  The computer doesn't care how fast or slow an algorithm runs.  But I think that horse is dead now.

So, given that we understand each other so well, let's talk about this issue of preference driven by environment.

In my work I don't care about nanoseconds.  I almost never care about microseconds.  I sometimes care about milliseconds.  Therefore I make the software engineering tradeoff towards _programmer convenience_, and long term readability and maintainability.  This means that I don't want to think about the hardware.  I don't want to know about Ln caches, or pipelining, or SIMD, or even how many cores there are in my processor.  I want all that abstracted away from me, and I am willing to spend billions of computer cycles to attain that abstraction and separation.  My concern is _programmer cycles_ not machine cycles.

I have, in the past, worn the other shoes.  There was a time in my career when microseconds had to be conserved, when I had to meet submillisecond deadlines, and I counted and conserved cycles as carefully as Ebenzer Scrooge.  So I think I understand your concern fairly well.

That is why I generally agreed with your video that kicked this whole discussion off but continue to disagree with that video's general conclusion that Clean Code is bad.

Let's set aside, for a moment, that the definition of "Clean Code" that you used in that video did not always align with the recommendations I made in my _Clean Code_ book.  The point is still valid.  Programmers who use the techniques that I recommended in that book _will be trading computer cycles for programmer cycles_.  And that's a good thing in most of the software teams and organizations that I work with.

I tip my hat, however, to your valid concern that myopically focussing on programmer cycles and utterly ignoring computer cycles can lead to horrifically inefficient systems.  None of us want that; just as none of us want to work in a world where we focus solely on computer cycles to the detriment of programmer productivity.

***

There are a gazillion other things we could go on to discuss.  We could argue about the dependency structure of switch statements vs polyomorphism.  We could talk about the best ways to cross architectural boundaries, and when link-time binding is better than run-time binding.  But, honestly, I think we have restored balance to _The Force_ and have achieved our primary goals.

So unless you have something you think is critically important to address, I recommend that we call an end to this dialog.

I'd like to thank you for a very stimulating, and civil, discussion.  I look forward to other such discussions in the future.

>_see cleancodeqa-2.md_

---

**CASEY**: I'm certainly happy to end the conversation here if you'd prefer. I appreciate you taking the time to do it, it's been rather lengthy :) But my closing analysis doesn't really line up with yours. So I'll include my response here for the record, but, if you'd rather not keep going, that's fine with me, too.

Essentially, I don't think our differences come down to preferences. I think the problem is that I'm not seeing where the developer cycles are being saved. I understand what you are _intending_ to do when you say that you want to trade CPU cycles for programmer cycles, but I don't see _where the actual savings come in_.

That's what I was trying to get at. Since we only just started talking about architecture, I tried to drill down on "dependency inversion" to see where you're seeing the tradeoff, because that was the one thing that you said was a _positive_ for your architecture style (runtime was the negative). But I don't feel like we ever got to the part where the benefit was made clear.

There was an example about file I/O, but as hopefully I demonstrated with the H/C file example, it does not require any new design, nor even any actual _work_, to simply cut the dependency at the interface boundary _no matter what_ the underlying architecture was. Any set of N function calls to a device driver can have the device driver swapped with another device driver, and now that driver implements those function calls. This is how it worked before OOP, and it's how it still works now in many file I/O subsystems. So I can't really use that to evaluate any "programmer cycle savings" for any specific architecture. The architectural benefits have to be demonstrated somewhere _below_ this cut, because there is not really any architecture I can think of that can't be cut in this way.

Now, thinking about the design of a file I/O subsystem, there certainly _are_ many things that I think are important, both in terms of "CPU cycles" and "programmer cycles". But in order to get into those you'd have to go beyond the basic premise that you want to be able to have read() calls that get fielded by different devices, OSes, or drivers.

If we end the discussion now, we also don't get a chance to talk about your payroll example, which was:

```
    public class Payroll {
      public void doPayroll(Date payDate) {
        for (Employee e : DB.getAllEmployees()) {
          if (isPayDay(e, payDate)) {
            Paycheck check = calculatePaycheck(e, payDate);
            pay(e, check);
          }
        }
      }
    }
```

You listed a bunch of things you wanted the implementation of `isPayDay()`/`calculatePaycheck()`/`pay()` to handle:

* Employees can be salaried, hourly, or commissioned
* Hourly employees are paid weekly
* Commissioned employees are paid twice a month
* Salaried employees are paid monthly
* Hourly employees are paid time and a half for overtime
* Commissioned employees are paid a base salary plus a commission
* Employees can get their paychecks directly deposited, mailed to their home, or held at the paymaster

That certainly sounds like a well-specified problem to me, so perhaps you can show me what operand-primal design you had in mind for the implementation that provides all these things and "saves developer cycles" over the operation-primal one? Because obviously, the code you wrote above _does not imply either one_. A programmer could do everything from inheritance hierarchies to a giant hard-coded calculatePaycheck function to satisfy your requirements here, so the savings in developer time is presumably coming from the _ease_ with which you can implement the underlying functions, and the corresponding ease with which you can make changes to them. Yes?

So if you _would_ like to elaborate on that design here, we can keep going. Or, if you've got a github where you implemented this example, you can point me to it and I'll go read it.

If not, no worries, we can call it a day.

---

**Bob**: I'm happy to continue.  I've moved this discussion to file -2.

You raised two topics above.  The saving of programmer cycles, and the utility of dynamic binding.  I think we need to separate these two concerns.

#### Programmer Cycles.
Regarding programmer cycles vs computer cycles.  Take a look at the file `tdox.c` in this respository.  Perhaps you've seen this before.  If not, if you compile and run it you'll get a cute surprise.

The point, of course, is that there are programming styles that can waste massive amounts of programmer cycles, and there are styles that concerve programmer cycles.  I have a style that I have refined through hard experience over the last 50 years; and I believe saves me cycles.  I recommend it to others based upon that experience and that belief. This style is based upon far more than just the occasional use of dynamic polymorphism.  Rather it focuses on function size, naming heuristics, code organization, dependency management, and quite a few other parameters.

Can I prove my belief?  Not mathematically; just as I am sure that you cannot mathematically prove that your favorite style saves more or less programmer cycles than mine.  All either of us can do is make _human_ arguments based upon our own particular perceptions of programmer psychology.  If you'd like to go there, we can certainly try.  But I fear we could get caught in a subjectivity trap.

#### Dyamic Binding
So let's first focus on the file I/O example.  Yes, of course, you can cut the dependency at the interface boundary and use link-time late binding to protect the applications from changes in the IO devices.  The switch statement (or if/else chain) that selects the IO device is kept in the OS and the application remains ignorant of it.  However, the OS has to be linked with all the drivers in order for the switch statement to be able to invoke them.  If we want to add a new driver, we need to link it into the OS and modify the switch statement accordingly.

Let us say, however, that we have a system that is continuously running and we cannot afford to take it off line to re-link the OS whenever a new IO driver is created.  Rather we want compile that new IO driver into a DLL, add it to a special directory, and let the OS hot load it.  That's a use case where dynamic binding comes in pretty handy.

Do we agree so far?

---

**CASEY**: Well, if we are just talking about the fact that at various points we want to use a dynamic linker in some way (meaning either we overwrite source code displacements or we patch a function table), then certainly we agree. Any time we want to load something dynamically, we need a dynamic linker, sort-of by definition (unless you are in a completely JIT-ed environment, which is very rare today).

So I don't disagree about that. But what I am trying to get at is which practices make the dynamically-linked system more or less likely to "save programmer cycles". I would argue - very strongly in this case specifically, but in most cases generally - that enums/flags and if/switches are much better than classes for this design. I think they are much better _along every axis_. They will be faster, easier to maintain, easier to read, easier to write, easier to debug, and result in a system that is easier for the user to work with as well.

But my _understanding_ - and feel free to correct me if this entire conversation is based on a misconception - is that you think classes with virtual functions will be the thing that does all of these _except_ perhaps the "faster" part (the aforementioned trade of CPU cycles for programmer cycles). While I agree there isn't a mathematical proof one way or the other, the purpose of a discussion like this is to get at the specifics, so seeing what each person calls a good implementation is very useful even if neither of us changes our minds. At least we _know_ specifically what the other person is talking about in practice, whereas right now I don't really know what the claimed savings is. Once we drill down to it, I may totally disagree that it's a savings, but at least I'll know _exactly_ what you're talking about, whereas at the moment I don't feel like I do.

Since the file example has gotten the most traction thus far, perhaps I can prevail upon you to show me what the design would be that you'd actually propose for something like this? Because the devil is in the details with things like programmer productivity, I'd like to keep it as real-world as possible, so perhaps we say that this is going to be a raw read/write API, so we do not have to include things like file metadata, to keep the scope small for purposes of discussion.

So perhaps we imagine a real modern operating system - say Microsoft Windows or Linux - is providing a new raw IO interface. It needs to:

* Allow the user to specify a particular raw device out of several that might be in the system.
* Allow the user to read n bytes from a specific offset on the device to a user-provided piece of memory
* Allow the user to write n bytes from a user-provided piece of memory to a specific offset on the device

What does this interface look like if designed properly according to your design principles?

---

**Bob**:  I guess that depends a lot on the language and the application. Java can read bytes from an `InputStream`.  In clojure I'd likely do something simple like slurp in the whole file and navigate in memory to the bytes I need.  But I think you are asking a different question.  I think you want me to continue the story I told about the '70s and the Unix style IO interface.

I'm not sure it's possible to improve upon that at the lowest levels.  The vtable of `open`, `close`, `read`, `write`, and `seek` are pretty good.  This scheme allows me to write the following program to copy one device to another without knowing what the device is:

	void copy() {
	  int c;
  	  while((c=getchar()) != EOF)
  		putchar(c)
	}

Where `getchar` and `putchar` are helper functions that call `read` and `write` on `stdin` and `stdout` repspectively.  If you compare this code to the kind of code that we had to write in the '60s to do roughly the same thing, the savings in programmer cycles is profound.

Does the above code depend upon dynamic polymorphism?  In UNIX it is certainly implemented that way.  But, as you pointed out earlier, it would be possible to create an OS interface that used link-time binding instead of run-time binding to accomplish the same thing -- with the exception that the IO drivers could not be dynamically loaded.

>_I could stop here and simply say that there are times when the link-time approach is better than the run-time approach; and that's true.  However, you said that enum/flags and if/switches are better in most cases. So let's examine that._

First we need to acknowledge that since the 90s there has been a very large emphasis on dynamic linking.  The creation of DLLs, Jars, and Shared Libraries moved the linker into the loader.  The reason this was considered valuable was so that we could build our applications using a plugin architecture.  You may remember `ActiveX` and DCOM, all that stuff from back then.  It ought to be clear that link-time binding that uses `if/switch` below the interface _impedes_ the plugin strategy; but let me explain why.

In the if/switch case, what does the OS look like?  Each of the five interface functions mentioned above probably has a switch statement in it.  It looks something like this in C(ish) code:

`file read.c`

	#include "devids.h"
	#include "console.h"
	#include "paper_tape.h"
	#include "..."
	#include "..."
	#include "..."
	#include "..."
	#include "..."
	void read(file* f, char* buf, int n) {
		switch(f->id) {
			case CONSOLE: read_console(f, buf, n); break;
			case PAPER_TAPE_READER: read_paper_tape(f, buf n); break;
			case...
			case...
			case...
			case...
			case...
		}
	}

This is a fairly ugly source file.  The more devices there are the bigger this file grows both in terms of `#include` and `case` statements.  The number of outgoing dependencies also grows, so this module has a very large _fan-out_.  And, remember, there are five of these files.

Whenever a new device is added, all five files must be updated with the new device.  Whenever any of the `#include`d header files change, all five files must be recompiled.

Where are the device IDs like `CONSOLE` and `PAPER_TAPE_READER` defined?  They are `#define` macros in the `devids.h` file.  Whenever a new device is added to the OS, the `devids.h` file must be modified to add the new `#define` macro.  And, of course, any source file in the OS that `#include`s `devids.h` will have to be recompiled.

What this means for the plugin architechture is that you can't simply write the plugin.  You must instead modify, and increase the _fan-out_ of, a large and growing source module, and somehow link that in with your new plugin.  It's possible to do this with DLLs/Jars/SharedLibs, but it's not fun.

It also introduces an administration headache.  In a multiple team environment, you've got to keep control over those switch statements.  You don't want to create _DLL-HELL_ by allowing multiple teams to have their own versions.

---

The vtable approach used by UNIX changes things around significantly.  The IO drivers can be loaded at any time.  When an IO device is selected the five functions are simply loaded into the `file`'s vtable.  There is no growing source file that steadily increases in _fan-out_.  There is no `devids.h` file to spur extra recompiles.  When new devices are added, nothing else has to be recompiled or relinked.  The DLL is simply loaded and registered into the set of devices.

---

Now the problems I've just outlined are quite common in modern applications.  It is not at all uncommon to see switch statement scattered throughout the body of the code -- all with the same cases but with different targets.  There can be a lot more than five; and they reproduce like gerbils.

But this is likely a good place to pause and say that I think there is a time and place for both link-time and run-time binding.  There is a time and place for both if/switch and dynamic polymorphism dispatch.

I think it is fair to say that I lean more towards the runtime polymorphism side of that divide.  But then I don't work in constrained environments.  Memory and cycles don't mean a lot to me anymore.  What matters most to me is source file organization and the minimization of the impact of source code dependencies.

---

Lastly, I just watched a video you did some time back on the principles of Reuse.  https://youtu.be/ZQ5_u8Lgvyk.  This is a great video.  It is densely packed with very good information that you presented very competently.

In that video you told the story of how you tried, and failed, to create a reusable framework.  You said that failure, and the subsequent more successful attempts, caused you to learn a lot.  That learning was the source of the principles you presented.

Your experience is eerily similar to one I had in the early '90s.  My team and I had 36 applications to write in C++.  We had limited time.  The applications had many similarities and we pitched a reusable framework to our customer.  We had no idea how hard this was going to be.  Our first attempt at this framework was developed in concert with ONE of the 36 applications.  It tooks us a very long time to create.  When we were done we tried to reuse that framework in four more applications, and failed horribly.  So we adopted a new strategy.  We stripped that framework down and rebuilt it; but only with elements that were used in all four of the applications we were writing.  Again, it took a long time, but when we started the next four applications, the framework fit like a glove; and we finished all 36 well in advance of our deadline.

We learned a lot from that experience.  That learning is one of the sources of the SOLID principles, and the Clean Code strategies that I recommend to others.

---

**CASEY**: I apologize for trying to be very specific here, but, I really want to actually get the exact proposal, and it wasn't really clear what it was. Could you tell me _exactly_ what the OS interface looks like and how it's implemented? You said _"I guess that depends a lot on the language and the application"_, but my understanding was that we were talking about the _OS_ side. Because obviously the application does not implement the raw device, so it doesn't need any switch statements or inheritance hierarchies or anything else. The OS and/or the driver implements the raw device, and it is expected to work across a wide range of applications and languages. So it can't "depend on the application or language" per se, it has to work with all of them. If you just mean it depends on what language the OS was written in, I assume it would be C/C++ because that's what Linux and Windows are written in at that layer, and I was hoping for this to be a real-world example.

In case it is not clear what I'm asking, we're talking about switch statements vs. inheritance hierarchies for implementing the layer from the OS down to the hardware. You said you thought this was a good example for where programmer cycles would be saved using inheritance vs. using switch statements. I assumed this meant that you believed having a particular inheritance hierarchy structure at the OS level would save either a) the OS programmers, b) the hardware vendors, or c) both, "programmer cycles", possibly at the expensve of CPU cycles, over the course of maintaining this OS. In my head, I don't see how that could possibly be the case vs. an enum/flags/switch/if approach. So I was hoping for an answer that spelled it out for me.

So what is the actual series of steps that happens here where the programmer cycles are saved? And I was hoping to then ask some hypotheticals, like about maintenance that would have to occur (adding a new device driver, etc.) Like, we start with the basic implemenation that they maybe had when they ship the first time, then we imagine the boss coming down and saying "now we need to support X", and then you can show me how it saves programmer cycles in that case, etc.

I'm assuming there is something like this somewhere?

```
class raw_device
{
public:
	virtual bool read(size_t Size, void *Memory) = 0;
	virtual bool write(size_t Size, void *Memory) = 0;
	// Is "close" here? "open"? A constructor or destructor? I'm not sure what you would advise.
};
```

Could you basically write what the base class is, and then how wiring works to go from the "getchar()"/"putchar()" example on the app side to the driver implementation of the derived class? That would be very helpful as a starting point so I can understand what you're actually recommending.

---

**Bob**: Woah, I had to update my RSA key for github.com before I could pull this.  See https://github.blog/2023-03-23-we-updated-our-rsa-ssh-host-key/

OK, so let's assume that we are writing a new OS.  And let's say that we are _not_ planning to hot-load the io drivers dynamically at runtime.  Instead we are going to statically link the OS.  Thus the only difference that we will consider between the switch and polymorphism solutions is programmer cycles.

In my previous response I showed what I thought the `switch` statement implementation might look like.
So here's what I think the dynamic polymorphism example might look like.

If we were writing this in C++ then there would likely be a base class defined as you showed above.

	#include "file.h"
	class raw_device {
	public:
		virtual file* open(char* name) = 0; // some other args here too elided for simplicity.
		virtual void close(file* f) = 0;
		virtual void read(file* f, size_t n, char* buf) = 0;
		virtual void write(file* f, size_t n, char* buf) = 0;
		virtual void seek(file* f, int n) = 0;
		virtual char* get_name() = 0; // return the name of this device.
	}

>_Forgive my archaic C++ style, it's been 20+ years since I wrote any serious C++ or C._

In this example the `raw_device` is really nothing more than a jump table.  It holds no data or state.  The `file` data structure holds the state of the currently open session. The `raw_device.h` file has a source code dependency on `file.h`; and will never have another source code dependency on anything else.  It has a _fan-out_ of 1.

Now I'd like to add an IO driver.  So I write the following code in `new_device.h`

	#include "raw_device.h"
	class new_device : public raw_device {
	public:
		virtual file* open(char* name);
		virtual void close(file* f);
		virtual void read(file* f, size_t n, char* buf);
		virtual void write(file* f, size_t n, char* buf);
		virtual void seek(file* f, int n);
		virtual void get_name();
	}

I also write the implementation in a .cc file `new_device.cc`:

	#include "new_device.h"

	file* new_device::open(char* name) {...}
	void new_device::close(file* f) {...}
	void new_device::read(file* f, size_t n, char* buf) {...}
	void new_device::write(file* f, size_t n, char* buf) {...}
	void new_device::seek(file* f int n) {...}
	void new_device::get_name() {return "new_device";}

These two source files have a direct _fan-out_ of 1 and a transitive _fan-out_ of 2.

Now we need a module to create the instances of our IO drivers and load them into a map of io devices keyed by their names.  My STL knowledge is a bit rusty so I'll make up a `map` class for this example.

	  io_driver_loader.cc
	  #include "new_device.h"
	  #include "new_device2.h"
	  #include "new_device3.h"
	  ...

	  void load_devices(device_map& map) {
		  map.add(new new_device()); // map.add uses get_name to associate the device with the name.
		  map.add(new new_device2());
		  map.add(new new_device3());
		  ...
	  }

>_I'm not compiling any of this so there are likely some syntax errors here and there that I hope you can forgive._

This last module has a _fan-out_ of N where N is the number of devices.

Whenever a new IO device must be added the new .h and .cc file must be created.  And the `io_driver_loader.cc` file must be modified.  They'll all have to be recompiled and relinked.

So let's count the programmer cycles in order to add a new device.

  * In the dynamic polymorpism case:
    * create `new_device_4.h`
    * create `new_device_4.cc`
    * modify `io_driver_loader.cc`
	* recompile and relink 3 files.

  * In the switch statement case:
    * create `new_device_4.h` forward declarations of the five functions.
	* create `new_device_4.cc` implementations of the five functions.
	* modify five switch statements, one for each of the functions, possibly in five different files.
	* modify `devids.h` to add the new `#define` macro for the device.
	* recompile and relink between 4 and 8 files, and all files that depend upon `devids.h`.

It should be clear that if we consider dynamic loading, then the `io_driver_loader.cc` file becomes something entirely different, and does not require modification when new IO devices are added; whereas the switch solution remains unchanged.

---

**CASEY**: Well, hold on, since I'm the switch statement proponent, I get to write the switch version :) But before we do that, I have a couple questions.

First, this is a raw device, so we don't actually need a file (I was trying to give us the simplest case so there would be as few details as possible!). So following your example I assume the base class in the DDK would be just:

```
	class raw_device {
	public:
		virtual void read(size_t offset, size_t n, char* buf) = 0;
		virtual void write(size_t offset, size_t n, char* buf) = 0;
		virtual char* get_name() = 0; // return the name of this device.
	}
```

and the implementation in the vendor's driver would be

```
	#include "raw_device.h" // from the DDK
	class new_device : public raw_device {
	public:
		virtual void read(size_t offset, size_t n, char* buf);
		virtual void write(size_t offset, size_t n, char* buf);
		virtual void get_name();
	}
```

Also, separate from the file handle part, I assume whenever one of these devices is used, it is looked up via the device map like this:

```
raw_device *find_raw_device(char *name) {
		raw_device *device = global_device_map[name];
		return device;
	  }
```

Is that correct? If you think it's important that the example include actual file handles, we can still do that, but if we do, I have other questions about the implementation.

---

**Bob**:
 * Regarding the switch:  Sure, no problem.
 * Regarding the file: My intent was that the file was an opaque pointer to _something_ that held the state of the session.  If you'd rather not have it, that's fine with me.  However, that reduces the number of operations from 5 down to 2 and impacts the count of programmer cycles.  But let's see where this goes.

---

**CASEY**: We can definitely keep file in there if you'd like. It's your choice. I just figured I'm about to ask a bunch of questions about how you modify the design to accomodate OS and device upgrades, and it seemed easier if it was the minimal function set to start with (we can add more, like OSes have to do from time to time). But if you'd like to have file in there as well, we can add it back in now. I'd just have some questions about it if it's still going to be in there.

Assuming we _don't_ add it back in, my first question would be, what does the user-side version of this look like? In other words, I assume what you've designed here is in the OS and driver internals. What API does the application programmer use to interact with a raw_device?

---

**Bob**:
A lot depends in the kind of OS we are using.  We could be talking about a big fat memory mapped OS like MACOS, or a little embedded RTOS.  So my description below will try to cover the range without getting too tangled.

Leaving the `file` out of things for the time being, and presuming the `class raw_device` that you presented above lives on the OS side, then the lowest level user API would likely be a linkable library that provides access to primitive delegator functions: `read` and `write`.  These functions would likely have signatures like:

 * `read(char* name, size_t offset, size_t n, char* buf);`
 * `write(char* name, size_t offset, size_t n, char* buf);`

These functions delegate to implementations that exist in the OS.  These implemementations use the `find_raw_device` function that you suggested to delegate to the `read` and `write` functions of the selected instance of `raw_device`.

Some means must be devised to implement the delegation across the OS boundary.  Some OSes might use special _interrupt_ or _system-call_ hardware instructions that the OS has provided vectors for.  Other OSes might create a vector table in some known part of memory.  Still others might simply declare the target functions to exist in a portion of memory that is memory mapped to a constant address.

Another possibility would be to avoid the delegation altogether by making the entire IO subsystem of the OS a linkable library.  (We see this approach used in embedded OSes). In this case the low level APIs would use the `class raw_device` and the `find_raw_device` function without the imposition of the `read` and `write` delegator functions and implementations I described above.

Above that primitive level of the API would be functions that are designed to be linked into the app, and that provide slightly higher level functionality.  For example, a function like `getchar` would be written in terms of calls to `read` (or in the embedded case direct calls to the `read` method of `raw_device`) and would use some means (such as environment variables) to translate `stdin` to the device name.

This is just one possible set of scenarios of course.  Others might involve exporting the vtables of the instances of `raw_device` for the current `stdin` and `stdout` devices so that they can be directly called by the application.  Indeed, this is close-ish to the approach used by some UNIX variants.

---

Having written all this, I fear we are getting fairly far afield from the actual topic, which is programmer-cycles vs. machine-cycles.  The example I presented was on the OS side only.  It seems to me that we'd be better off investigating whether that approach saves programmer cycles for the OS developers rather than worrying about the app developers on the other side of the OS boundary.

Or perhaps we should focus on the embedded case where the OS and the app are linked together into a single executable.  In that case the OS developers and the app developers are harder to separate since the app developers will likely need to write their own device drivers.

---

OR -- perhaps we've gone down a rathole and it's time to cut to the chase.  See `programmer-cycles-vs-machine-cycles.md` file that I just created in this repository.

As much as I've enjoyed this discussion, and as much as I've come to appreciate your knowledge and experience, I'm coming to the end of the time I can give to it.  So if our current thread is headed somewhere specific then let's get there without further delay.

---

**CASEY**: I'm not even talking about machine-cycles, I was _just_ focusing on programmer-cycles. But I understand your design now, which I will summarize here for clarity.

First, every operation is represented by a function, 2+d times (once in userland, once in the kernel, then in d drivers). So for o operations, we have (2+d)\*o pieces of code, generally speaking.

On the application side we have:

```
int read(char* name, size_t offset, size_t n, char* buf);
int write(char* name, size_t offset, size_t n, char* buf);
```

These operate in userland and then they thunk down to ring-0 versions that look like (if I understand correctly):

```
int read_internal(char* name, size_t offset, size_t n, char* buf)
{
	// Translating addresses and checking bounds goes here, for "name" and "buf"

	int Error = DEVICE_NOT_FOUND;
	raw_device *Dev = find_device(name);
	if(Dev)
	{
		Dev->read(offset, n, buf);
	}

	return Error;
}
```

with raw_device being this:

```
	class raw_device {
	public:
		virtual void read(size_t offset, size_t n, char* buf) = 0;
		virtual void write(size_t offset, size_t n, char* buf) = 0;
		virtual char* get_name() = 0; // return the name of this device.
	}
```

So, for comparison, let's look at what an enumerant-based solution would be. In this scenario, there are no practical limits I can think of in breaking things down into numbers, so I would do it for literally everything (the devices _and_ the operations):

```
// Some people prefer namespaces and such, so if that is your thing, imagine these are wrapped for naming however you prefer

enum raw_device_operation : u32
{
	RIO_none,

	RIO_read,
	RIO_write,
	RIO_get_name,

	RIO_private = 0x80000000, // Completely optional - I would do it, but then again, maybe I wouldn't if these drivers are ring-0 and we don't trust them much?
};

struct raw_device_id
{
	u32 ID;
};

struct raw_device_request
{
	size_t Offset;
	size_t Size;
	void *Buffer;
	raw_device_operation OP;
	raw_device_id Device;

	// Anything in an OS like this I typically want to pad to cache line size, in case there is multithreading going on:
	u64 Reserved64[4];
};

strcut raw_device_result
{
	u32 error_code;

	// Same as above - maybe this is overkill, but, I don't like cache-unaligned things, so, force of habit.
	u32 Reserved32;
	u64 Reserved64[7];
};
```

Now, I don't like vtables or language-specific object stuff pretty much anywhere, because I find it harder ---to control, so I would prefer

**CASEY**: I'm certainly happy to end the conversation here if you'd prefer. I appreciate you taking the time to do it, it's been rather lengthy :) But my closing analysis doesn't really line up with yours. So I'll include my response here for the record, but, if you'd rather not keep going, that's fine with me, too.

Essentially, I don't think our differences come down to preferences. I think the problem is that I'm not seeing where the developer cycles are being saved. I understand what you are _intending_ to do when you say that you want to trade CPU cycles for programmer cycles, but I don't see _where the actual savings come in_.

That's what I was trying to get at. Since we only just started talking about architecture, I tried to drill down on "dependency inversion" to see where you're seeing the tradeoff, because that was the one thing that you said was a _positive_ for your architecture style (runtime was the negative). But I don't feel like we ever got to the part where the benefit was made clear.

There was an example about file I/O, but as hopefully I demonstrated with the H/C file example, it does not require any new design, nor even any actual _work_, to simply cut the dependency at the interface boundary _no matter what_ the underlying architecture was. Any set of N function calls to a device driver can have the device driver swapped with another device driver, and now that driver implements those function calls. This is how it worked before OOP, and it's how it still works now in many file I/O subsystems. So I can't really use that to evaluate any "programmer cycle savings" for any specific architecture. The architectural benefits have to be demonstrated somewhere _below_ this cut, because there is not really any architecture I can think of that can't be cut in this way.

Now, thinking about the design of a file I/O subsystem, there certainly _are_ many things that I think are important, both in terms of "CPU cycles" and "programmer cycles". But in order to get into those you'd have to go beyond the basic premise that you want to be able to have read() calls that get fielded by different devices, OSes, or drivers.

If we end the discussion now, we also don't get a chance to talk about your payroll example, which was:

```
    public class Payroll {
      public void doPayroll(Date payDate) {
        for (Employee e : DB.getAllEmployees()) {
          if (isPayDay(e, payDate)) {
            Paycheck check = calculatePaycheck(e, payDate);
            pay(e, check);
          }
        }
      }
    }
```

You listed a bunch of things you wanted the implementation of `isPayDay()`/`calculatePaycheck()`/`pay()` to handle:

* Employees can be salaried, hourly, or commissioned
* Hourly employees are paid weekly
* Commissioned employees are paid twice a month
* Salaried employees are paid monthly
* Hourly employees are paid time and a half for overtime
* Commissioned employees are paid a base salary plus a commission
* Employees can get their paychecks directly deposited, mailed to their home, or held at the paymaster

That certainly sounds like a well-specified problem to me, so perhaps you can show me what operand-primal design you had in mind for the implementation that provides all these things and "saves developer cycles" over the operation-primal one? Because obviously, the code you wrote above _does not imply either one_. A programmer could do everything from inheritance hierarchies to a giant hard-coded calculatePaycheck function to satisfy your requirements here, so the savings in developer time is presumably coming from the _ease_ with which you can implement the underlying functions, and the corresponding ease with which you can make changes to them. Yes?

So if you _would_ like to elaborate on that design here, we can keep going. Or, if you've got a github where you implemented this example, you can point me to it and I'll go read it.

If not, no worries, we can call it a day.

---

**Bob**: I'm happy to continue.  I've moved this discussion to file -2.

You raised two topics above.  The saving of programmer cycles, and the utility of dynamic binding.  I think we need to separate these two concerns.

#### Programmer Cycles.
Regarding programmer cycles vs computer cycles.  Take a look at the file `tdox.c` in this respository.  Perhaps you've seen this before.  If not, if you compile and run it you'll get a cute surprise.

The point, of course, is that there are programming styles that can waste massive amounts of programmer cycles, and there are styles that concerve programmer cycles.  I have a style that I have refined through hard experience over the last 50 years; and I believe saves me cycles.  I recommend it to others based upon that experience and that belief. This style is based upon far more than just the occasional use of dynamic polymorphism.  Rather it focuses on function size, naming heuristics, code organization, dependency management, and quite a few other parameters.

Can I prove my belief?  Not mathematically; just as I am sure that you cannot mathematically prove that your favorite style saves more or less programmer cycles than mine.  All either of us can do is make _human_ arguments based upon our own particular perceptions of programmer psychology.  If you'd like to go there, we can certainly try.  But I fear we could get caught in a subjectivity trap.

#### Dyamic Binding
So let's first focus on the file I/O example.  Yes, of course, you can cut the dependency at the interface boundary and use link-time late binding to protect the applications from changes in the IO devices.  The switch statement (or if/else chain) that selects the IO device is kept in the OS and the application remains ignorant of it.  However, the OS has to be linked with all the drivers in order for the switch statement to be able to invoke them.  If we want to add a new driver, we need to link it into the OS and modify the switch statement accordingly.

Let us say, however, that we have a system that is continuously running and we cannot afford to take it off line to re-link the OS whenever a new IO driver is created.  Rather we want compile that new IO driver into a DLL, add it to a special directory, and let the OS hot load it.  That's a use case where dynamic binding comes in pretty handy.

Do we agree so far?

---

**CASEY**: Well, if we are just talking about the fact that at various points we want to use a dynamic linker in some way (meaning either we overwrite source code displacements or we patch a function table), then certainly we agree. Any time we want to load something dynamically, we need a dynamic linker, sort-of by definition (unless you are in a completely JIT-ed environment, which is very rare today).

So I don't disagree about that. But what I am trying to get at is which practices make the dynamically-linked system more or less likely to "save programmer cycles". I would argue - very strongly in this case specifically, but in most cases generally - that enums/flags and if/switches are much better than classes for this design. I think they are much better _along every axis_. They will be faster, easier to maintain, easier to read, easier to write, easier to debug, and result in a system that is easier for the user to work with as well.

But my _understanding_ - and feel free to correct me if this entire conversation is based on a misconception - is that you think classes with virtual functions will be the thing that does all of these _except_ perhaps the "faster" part (the aforementioned trade of CPU cycles for programmer cycles). While I agree there isn't a mathematical proof one way or the other, the purpose of a discussion like this is to get at the specifics, so seeing what each person calls a good implementation is very useful even if neither of us changes our minds. At least we _know_ specifically what the other person is talking about in practice, whereas right now I don't really know what the claimed savings is. Once we drill down to it, I may totally disagree that it's a savings, but at least I'll know _exactly_ what you're talking about, whereas at the moment I don't feel like I do.

Since the file example has gotten the most traction thus far, perhaps I can prevail upon you to show me what the design would be that you'd actually propose for something like this? Because the devil is in the details with things like programmer productivity, I'd like to keep it as real-world as possible, so perhaps we say that this is going to be a raw read/write API, so we do not have to include things like file metadata, to keep the scope small for purposes of discussion.

So perhaps we imagine a real modern operating system - say Microsoft Windows or Linux - is providing a new raw IO interface. It needs to:

* Allow the user to specify a particular raw device out of several that might be in the system.
* Allow the user to read n bytes from a specific offset on the device to a user-provided piece of memory
* Allow the user to write n bytes from a user-provided piece of memory to a specific offset on the device

What does this interface look like if designed properly according to your design principles?

---

**Bob**:  I guess that depends a lot on the language and the application. Java can read bytes from an `InputStream`.  In clojure I'd likely do something simple like slurp in the whole file and navigate in memory to the bytes I need.  But I think you are asking a different question.  I think you want me to continue the story I told about the '70s and the Unix style IO interface.

I'm not sure it's possible to improve upon that at the lowest levels.  The vtable of `open`, `close`, `read`, `write`, and `seek` are pretty good.  This scheme allows me to write the following program to copy one device to another without knowing what the device is:

	void copy() {
	  int c;
  	  while((c=getchar()) != EOF)
  		putchar(c)
	}

Where `getchar` and `putchar` are helper functions that call `read` and `write` on `stdin` and `stdout` repspectively.  If you compare this code to the kind of code that we had to write in the '60s to do roughly the same thing, the savings in programmer cycles is profound.

Does the above code depend upon dynamic polymorphism?  In UNIX it is certainly implemented that way.  But, as you pointed out earlier, it would be possible to create an OS interface that used link-time binding instead of run-time binding to accomplish the same thing -- with the exception that the IO drivers could not be dynamically loaded.

>_I could stop here and simply say that there are times when the link-time approach is better than the run-time approach; and that's true.  However, you said that enum/flags and if/switches are better in most cases. So let's examine that._

First we need to acknowledge that since the 90s there has been a very large emphasis on dynamic linking.  The creation of DLLs, Jars, and Shared Libraries moved the linker into the loader.  The reason this was considered valuable was so that we could build our applications using a plugin architecture.  You may remember `ActiveX` and DCOM, all that stuff from back then.  It ought to be clear that link-time binding that uses `if/switch` below the interface _impedes_ the plugin strategy; but let me explain why.

In the if/switch case, what does the OS look like?  Each of the five interface functions mentioned above probably has a switch statement in it.  It looks something like this in C(ish) code:

`file read.c`

	#include "devids.h"
	#include "console.h"
	#include "paper_tape.h"
	#include "..."
	#include "..."
	#include "..."
	#include "..."
	#include "..."
	void read(file* f, char* buf, int n) {
		switch(f->id) {
			case CONSOLE: read_console(f, buf, n); break;
			case PAPER_TAPE_READER: read_paper_tape(f, buf n); break;
			case...
			case...
			case...
			case...
			case...
		}
	}

This is a fairly ugly source file.  The more devices there are the bigger this file grows both in terms of `#include` and `case` statements.  The number of outgoing dependencies also grows, so this module has a very large _fan-out_.  And, remember, there are five of these files.

Whenever a new device is added, all five files must be updated with the new device.  Whenever any of the `#include`d header files change, all five files must be recompiled.

Where are the device IDs like `CONSOLE` and `PAPER_TAPE_READER` defined?  They are `#define` macros in the `devids.h` file.  Whenever a new device is added to the OS, the `devids.h` file must be modified to add the new `#define` macro.  And, of course, any source file in the OS that `#include`s `devids.h` will have to be recompiled.

What this means for the plugin architechture is that you can't simply write the plugin.  You must instead modify, and increase the _fan-out_ of, a large and growing source module, and somehow link that in with your new plugin.  It's possible to do this with DLLs/Jars/SharedLibs, but it's not fun.

It also introduces an administration headache.  In a multiple team environment, you've got to keep control over those switch statements.  You don't want to create _DLL-HELL_ by allowing multiple teams to have their own versions.

---

The vtable approach used by UNIX changes things around significantly.  The IO drivers can be loaded at any time.  When an IO device is selected the five functions are simply loaded into the `file`'s vtable.  There is no growing source file that steadily increases in _fan-out_.  There is no `devids.h` file to spur extra recompiles.  When new devices are added, nothing else has to be recompiled or relinked.  The DLL is simply loaded and registered into the set of devices.

---

Now the problems I've just outlined are quite common in modern applications.  It is not at all uncommon to see switch statement scattered throughout the body of the code -- all with the same cases but with different targets.  There can be a lot more than five; and they reproduce like gerbils.

But this is likely a good place to pause and say that I think there is a time and place for both link-time and run-time binding.  There is a time and place for both if/switch and dynamic polymorphism dispatch.

I think it is fair to say that I lean more towards the runtime polymorphism side of that divide.  But then I don't work in constrained environments.  Memory and cycles don't mean a lot to me anymore.  What matters most to me is source file organization and the minimization of the impact of source code dependencies.

---

Lastly, I just watched a video you did some time back on the principles of Reuse.  https://youtu.be/ZQ5_u8Lgvyk.  This is a great video.  It is densely packed with very good information that you presented very competently.

In that video you told the story of how you tried, and failed, to create a reusable framework.  You said that failure, and the subsequent more successful attempts, caused you to learn a lot.  That learning was the source of the principles you presented.

Your experience is eerily similar to one I had in the early '90s.  My team and I had 36 applications to write in C++.  We had limited time.  The applications had many similarities and we pitched a reusable framework to our customer.  We had no idea how hard this was going to be.  Our first attempt at this framework was developed in concert with ONE of the 36 applications.  It tooks us a very long time to create.  When we were done we tried to reuse that framework in four more applications, and failed horribly.  So we adopted a new strategy.  We stripped that framework down and rebuilt it; but only with elements that were used in all four of the applications we were writing.  Again, it took a long time, but when we started the next four applications, the framework fit like a glove; and we finished all 36 well in advance of our deadline.

We learned a lot from that experience.  That learning is one of the sources of the SOLID principles, and the Clean Code strategies that I recommend to others.

---

**CASEY**: I apologize for trying to be very specific here, but, I really want to actually get the exact proposal, and it wasn't really clear what it was. Could you tell me _exactly_ what the OS interface looks like and how it's implemented? You said _"I guess that depends a lot on the language and the application"_, but my understanding was that we were talking about the _OS_ side. Because obviously the application does not implement the raw device, so it doesn't need any switch statements or inheritance hierarchies or anything else. The OS and/or the driver implements the raw device, and it is expected to work across a wide range of applications and languages. So it can't "depend on the application or language" per se, it has to work with all of them. If you just mean it depends on what language the OS was written in, I assume it would be C/C++ because that's what Linux and Windows are written in at that layer, and I was hoping for this to be a real-world example.

In case it is not clear what I'm asking, we're talking about switch statements vs. inheritance hierarchies for implementing the layer from the OS down to the hardware. You said you thought this was a good example for where programmer cycles would be saved using inheritance vs. using switch statements. I assumed this meant that you believed having a particular inheritance hierarchy structure at the OS level would save either a) the OS programmers, b) the hardware vendors, or c) both, "programmer cycles", possibly at the expensve of CPU cycles, over the course of maintaining this OS. In my head, I don't see how that could possibly be the case vs. an enum/flags/switch/if approach. So I was hoping for an answer that spelled it out for me.

So what is the actual series of steps that happens here where the programmer cycles are saved? And I was hoping to then ask some hypotheticals, like about maintenance that would have to occur (adding a new device driver, etc.) Like, we start with the basic implemenation that they maybe had when they ship the first time, then we imagine the boss coming down and saying "now we need to support X", and then you can show me how it saves programmer cycles in that case, etc.

I'm assuming there is something like this somewhere?

```
class raw_device
{
public:
	virtual bool read(size_t Size, void *Memory) = 0;
	virtual bool write(size_t Size, void *Memory) = 0;
	// Is "close" here? "open"? A constructor or destructor? I'm not sure what you would advise.
};
```

Could you basically write what the base class is, and then how wiring works to go from the "getchar()"/"putchar()" example on the app side to the driver implementation of the derived class? That would be very helpful as a starting point so I can understand what you're actually recommending.

---

**Bob**: Woah, I had to update my RSA key for github.com before I could pull this.  See https://github.blog/2023-03-23-we-updated-our-rsa-ssh-host-key/

OK, so let's assume that we are writing a new OS.  And let's say that we are _not_ planning to hot-load the io drivers dynamically at runtime.  Instead we are going to statically link the OS.  Thus the only difference that we will consider between the switch and polymorphism solutions is programmer cycles.

In my previous response I showed what I thought the `switch` statement implementation might look like.
So here's what I think the dynamic polymorphism example might look like.

If we were writing this in C++ then there would likely be a base class defined as you showed above.

	#include "file.h"
	class raw_device {
	public:
		virtual file* open(char* name) = 0; // some other args here too elided for simplicity.
		virtual void close(file* f) = 0;
		virtual void read(file* f, size_t n, char* buf) = 0;
		virtual void write(file* f, size_t n, char* buf) = 0;
		virtual void seek(file* f, int n) = 0;
		virtual char* get_name() = 0; // return the name of this device.
	}

>_Forgive my archaic C++ style, it's been 20+ years since I wrote any serious C++ or C._

In this example the `raw_device` is really nothing more than a jump table.  It holds no data or state.  The `file` data structure holds the state of the currently open session. The `raw_device.h` file has a source code dependency on `file.h`; and will never have another source code dependency on anything else.  It has a _fan-out_ of 1.

Now I'd like to add an IO driver.  So I write the following code in `new_device.h`

	#include "raw_device.h"
	class new_device : public raw_device {
	public:
		virtual file* open(char* name);
		virtual void close(file* f);
		virtual void read(file* f, size_t n, char* buf);
		virtual void write(file* f, size_t n, char* buf);
		virtual void seek(file* f, int n);
		virtual void get_name();
	}

I also write the implementation in a .cc file `new_device.cc`:

	#include "new_device.h"

	file* new_device::open(char* name) {...}
	void new_device::close(file* f) {...}
	void new_device::read(file* f, size_t n, char* buf) {...}
	void new_device::write(file* f, size_t n, char* buf) {...}
	void new_device::seek(file* f int n) {...}
	void new_device::get_name() {return "new_device";}

These two source files have a direct _fan-out_ of 1 and a transitive _fan-out_ of 2.

Now we need a module to create the instances of our IO drivers and load them into a map of io devices keyed by their names.  My STL knowledge is a bit rusty so I'll make up a `map` class for this example.

	  io_driver_loader.cc
	  #include "new_device.h"
	  #include "new_device2.h"
	  #include "new_device3.h"
	  ...

	  void load_devices(device_map& map) {
		  map.add(new new_device()); // map.add uses get_name to associate the device with the name.
		  map.add(new new_device2());
		  map.add(new new_device3());
		  ...
	  }

>_I'm not compiling any of this so there are likely some syntax errors here and there that I hope you can forgive._

This last module has a _fan-out_ of N where N is the number of devices.

Whenever a new IO device must be added the new .h and .cc file must be created.  And the `io_driver_loader.cc` file must be modified.  They'll all have to be recompiled and relinked.

So let's count the programmer cycles in order to add a new device.

  * In the dynamic polymorpism case:
    * create `new_device_4.h`
    * create `new_device_4.cc`
    * modify `io_driver_loader.cc`
	* recompile and relink 3 files.

  * In the switch statement case:
    * create `new_device_4.h` forward declarations of the five functions.
	* create `new_device_4.cc` implementations of the five functions.
	* modify five switch statements, one for each of the functions, possibly in five different files.
	* modify `devids.h` to add the new `#define` macro for the device.
	* recompile and relink between 4 and 8 files, and all files that depend upon `devids.h`.

It should be clear that if we consider dynamic loading, then the `io_driver_loader.cc` file becomes something entirely different, and does not require modification when new IO devices are added; whereas the switch solution remains unchanged.

---

**CASEY**: Well, hold on, since I'm the switch statement proponent, I get to write the switch version :) But before we do that, I have a couple questions.

First, this is a raw device, so we don't actually need a file (I was trying to give us the simplest case so there would be as few details as possible!). So following your example I assume the base class in the DDK would be just:

```
	class raw_device {
	public:
		virtual void read(size_t offset, size_t n, char* buf) = 0;
		virtual void write(size_t offset, size_t n, char* buf) = 0;
		virtual char* get_name() = 0; // return the name of this device.
	}
```

and the implementation in the vendor's driver would be

```
	#include "raw_device.h" // from the DDK
	class new_device : public raw_device {
	public:
		virtual void read(size_t offset, size_t n, char* buf);
		virtual void write(size_t offset, size_t n, char* buf);
		virtual void get_name();
	}
```

Also, separate from the file handle part, I assume whenever one of these devices is used, it is looked up via the device map like this:

```
raw_device *find_raw_device(char *name) {
		raw_device *device = global_device_map[name];
		return device;
	  }
```

Is that correct? If you think it's important that the example include actual file handles, we can still do that, but if we do, I have other questions about the implementation.

---

**Bob**:
 * Regarding the switch:  Sure, no problem.
 * Regarding the file: My intent was that the file was an opaque pointer to _something_ that held the state of the session.  If you'd rather not have it, that's fine with me.  However, that reduces the number of operations from 5 down to 2 and impacts the count of programmer cycles.  But let's see where this goes.

---

**CASEY**: We can definitely keep file in there if you'd like. It's your choice. I just figured I'm about to ask a bunch of questions about how you modify the design to accomodate OS and device upgrades, and it seemed easier if it was the minimal function set to start with (we can add more, like OSes have to do from time to time). But if you'd like to have file in there as well, we can add it back in now. I'd just have some questions about it if it's still going to be in there.

Assuming we _don't_ add it back in, my first question would be, what does the user-side version of this look like? In other words, I assume what you've designed here is in the OS and driver internals. What API does the application programmer use to interact with a raw_device?

---

**Bob**:
A lot depends in the kind of OS we are using.  We could be talking about a big fat memory mapped OS like MACOS, or a little embedded RTOS.  So my description below will try to cover the range without getting too tangled.

Leaving the `file` out of things for the time being, and presuming the `class raw_device` that you presented above lives on the OS side, then the lowest level user API would likely be a linkable library that provides access to primitive delegator functions: `read` and `write`.  These functions would likely have signatures like:

 * `read(char* name, size_t offset, size_t n, char* buf);`
 * `write(char* name, size_t offset, size_t n, char* buf);`

These functions delegate to implementations that exist in the OS.  These implemementations use the `find_raw_device` function that you suggested to delegate to the `read` and `write` functions of the selected instance of `raw_device`.

Some means must be devised to implement the delegation across the OS boundary.  Some OSes might use special _interrupt_ or _system-call_ hardware instructions that the OS has provided vectors for.  Other OSes might create a vector table in some known part of memory.  Still others might simply declare the target functions to exist in a portion of memory that is memory mapped to a constant address.

Another possibility would be to avoid the delegation altogether by making the entire IO subsystem of the OS a linkable library.  (We see this approach used in embedded OSes). In this case the low level APIs would use the `class raw_device` and the `find_raw_device` function without the imposition of the `read` and `write` delegator functions and implementations I described above.

Above that primitive level of the API would be functions that are designed to be linked into the app, and that provide slightly higher level functionality.  For example, a function like `getchar` would be written in terms of calls to `read` (or in the embedded case direct calls to the `read` method of `raw_device`) and would use some means (such as environment variables) to translate `stdin` to the device name.

This is just one possible set of scenarios of course.  Others might involve exporting the vtables of the instances of `raw_device` for the current `stdin` and `stdout` devices so that they can be directly called by the application.  Indeed, this is close-ish to the approach used by some UNIX variants.

---

Having written all this, I fear we are getting fairly far afield from the actual topic, which is programmer-cycles vs. machine-cycles.  The example I presented was on the OS side only.  It seems to me that we'd be better off investigating whether that approach saves programmer cycles for the OS developers rather than worrying about the app developers on the other side of the OS boundary.

Or perhaps we should focus on the embedded case where the OS and the app are linked together into a single executable.  In that case the OS developers and the app developers are harder to separate since the app developers will likely need to write their own device drivers.

---

OR -- perhaps we've gone down a rathole and it's time to cut to the chase.  See `programmer-cycles-vs-machine-cycles.md` file that I just created in this repository.

As much as I've enjoyed this discussion, and as much as I've come to appreciate your knowledge and experience, I'm coming to the end of the time I can give to it.  So if our current thread is headed somewhere specific then let's get there without further delay.

---

**CASEY**: I'm not even talking about machine-cycles, I was _just_ focusing on programmer-cycles. But I understand your design now, which I will summarize here for clarity.

First, every operation is represented by a function, 2+d times (once in userland, once in the kernel, then in d drivers). So for o operations, we have (2+d)\*o pieces of code, generally speaking.

On the application side we have:

```
int read(char* name, size_t offset, size_t n, char* buf);
int write(char* name, size_t offset, size_t n, char* buf);
```

These operate in userland and then they thunk down to ring-0 versions that look like (if I understand correctly):

```
int read_internal(char* name, size_t offset, size_t n, char* buf)
{
	// Translating addresses and checking bounds goes here, for "name" and "buf"

	int Error = DEVICE_NOT_FOUND;
	raw_device *Dev = find_device(name);
	if(Dev)
	{
		Dev->read(offset, n, buf);
	}

	return Error;
}
```

with raw_device being this:

```
	class raw_device {
	public:
		virtual void read(size_t offset, size_t n, char* buf) = 0;
		virtual void write(size_t offset, size_t n, char* buf) = 0;
		virtual char* get_name() = 0; // return the name of this device.
	}
```

So, for comparison, let's look at what an enumerant-based solution would be. In this scenario, there are no practical limits I can think of in breaking things down into numbers, so I would do it for literally everything (the devices _and_ the operations):

```
// Some people prefer namespaces and such, so if that is your thing, imagine these are wrapped for naming however you prefer

enum raw_device_operation : u32
{
	RIO_none,

	RIO_read,
	RIO_write,
	RIO_get_name,

	RIO_private = 0x80000000, // Completely optional - I would do it, but then again, maybe I wouldn't if these drivers are ring-0 and we don't trust them much?
};

struct raw_device_id
{
	u32 ID;
};

struct raw_device_request
{
	size_t Offset;
	size_t Size;
	void *Buffer;
	raw_device_operation OP;
	raw_device_id Device;

	// Anything in an OS like this I typically want to pad to cache line size, in case there is multithreading going on:
	u64 Reserved64[4];
};

strcut raw_device_result
{
	u32 error_code;

	// Same as above - maybe this is overkill, but, I don't like cache-unaligned things, so, force of habit.
	u32 Reserved32;
	u64 Reserved64[7];
};
```

Now, I don't like vtables or language-specific object stuff pretty much anywhere, because I find it harder to control, so I would prefer

```
typedef void raw_device_handler(u32 Instance, raw_device_request *Packet, raw_device_result *Result);
struct raw_device
{
	raw_device_handler *Handler;
};
```

instead of the class version. But the only real reason I have to write that is, of course, because C++ is crappy and you can't say "I would like this class to contain function pointers, instead of a pointer to a table of function pointers, please." Otherwise, I wouldn't have a problem with the class version. So, although I would rather have the direct pointer version in practice, for purposes of this discussion (since we only care about programmer cycles right now), you can ignore my raw_device struct and we can just assume that it is written in class form like yours since I can't think of any programmer-cycles benefit to my version in this case:

```
class raw_device
{
public:
	void Handler(raw_device_request *Packet, raw_device_result *Result);
};
```

Since the user never sees a raw_device - they would just be using the 32-bit raw_device_id handle - leaving this as a class is fine.

When someone implements a device driver, they really just implement one function:

```
void raw_device::Handler(raw_device_request *Packet, raw_device_result *Result)
{
	switch(Packet->Op)
	{
		case RIO_read:
		// etc.

		case RIO_write:
		// etc.

		case RIO_get_name:
		// etc.

		default:
		// write error Result
	}
}
```

Obviously, the user-side version of this can be the same as yours:

```
int read(char* name, size_t offset, size_t n, char* buf);
int write(char* name, size_t offset, size_t n, char* buf);
```

However, I might recommend that it be more like:

```
raw_device_id lookup_device(char* name);
int read(raw_device_id device, size_t offset, size_t n, char* buf);
int write(raw_device_id device, size_t offset, size_t n, char* buf);
```

in _either_ case, but that is largely irrelevant, because that's solely about the lookup to map to a specific device, and can be used with either design.

Anyway, over the course of the development of the OS, I think this implementation _saves_ programmer cycles, potentially a lot of them, compared to the one I understand to be favored by the "Clean Code" method (which is also the one you wrote above). Again, in my understanding, the reason that yours looks the way it looks is specifically because of two design ideas that you mention often in books and in lectures:

* You favor having one class for each type of thing (in this case a driver), with one virtual member function per operation that the thing does (in this case read, write, and get_name)
* Operation permutations are not supposed to pass through functions - so passing an enum to a function that says what to do is bad, you should have one function per thing that gets done. I've seen this mentioned multiple times - not just for switch statements, but also when you say functions with "if" statements on function parameters in them, that modifies what the function does, you're supposed to rewrite that as two functions.

In my experience, these two things cost a lot of programmer cycles. I prefer to collapse things into functions with parameters when possible, because it reduces the total number of things that the programmer has to think about, and reduces the number of files, lines of code, and data permutations they have to consider. It allows functions to "pass through" information about what is happening without knowing what it is. Again, I tend to think of this as "operation-primal", because instead of focusing on making your _types_ polymorphic, you're making your _functions_ polymorphic.

I don't know if "abstracting file IO" is the best example to pick, but it was the first one you brought up, and it _happens_ to contrast the two designs enough in my opinion, so it works for me. Here's why I think an enum-based design saves the programmer(s) cycles:

* In most systems, we do not know all the functions that we are going to do ahead of time. When operating across a hard boundary like a driver, using operation codes instead of virtual function calls allows us to add functions dynamically without recompiling all of our drivers. If we want, for example, to add a new function like "trim" (such as what happened with real IO protocols when SSDs happened), if we are just passing opcodes to a driver, we don't have to recompile _anything_. All the old drivers just ignore the code, whereas the new drivers act on it, which is what we want. In the class case, either all the drivers have to be recompiled, or we have to write a utility class which stubs the new function, and wrap all the old drivers in that utility class. If we don't do this, the vtables for the old drivers will all be the wrong size, so we can't use them.
* In any modern system, multithreading is a concern, but this is especially true for an operating system. Having the protocol be structure-based, with an operation code, allows us to trivially buffer operations in things like io rings or other intermediaries _without writing any new code_. The entire system remains identical, and we need only add the new buffer logic. In the class system, we don't have any way to represent function operations with data, so wse must _introduce_ a data format for storing the calls - which of course will end up looking basically like the code for the non-class version, so you end up having to write my version _as well as_ the class version. This, by the way, seems to happen in almost _all_ OOP systems I see, because eventually they need to serialize or something similar, and so they have to write my version _as well as_ their version, but they don't seem to realize how much time they're wasting this way!
* If at some point we decide that users should be able to do multithreaded/bulk IO ops, with the enum version there are _zero_ changes to the internals and the drivers. All we do is add whatever user-land interface we want for this (presumably a circular buffer they write to), and everything "just works", with no translation necessary because the internals already worked on data instead of vtables/function calls anyway.
* If we would like to allow 3rd parties to have private communication channels for their devices, where user-level code can do IO ops that are only defined on those devices, there is no way to do this if we stick with the idea that each operation has to be represented by its own virtual function. It requires a complete side-channel implementation that has to be created specially and deployed by each vendor separately, because we have no idea how many functions each device will need or what they will be called. With the enum version, it's trivial. Just reserve half the opcode range for private stuff, and let anything in that range pass through to the device. Now the vendor ships an enum table to the end users, or an SDK with function calls that write those enums for them if you prefer, and that's it. Whether or not you'd actually _want_ to do this in an OS is questionable, because you're trusting the driver more here, but the point is the architecture _makes it trivial_. Whether or not you should allow it on security grounds is another question, and may depend on the type of OS.

Also, I should mention, that all of those things I just listed were _actual_ things that happened to IO subsystems in OS design. So none of those are hypothetical things that don't usually occur, or which are cherry-picked to be beneficial to enum-based design. Those are basically just _the exact things that have happened_ over the past ~20 years of OS IO subsystem evolution in a broad sense.

So what is really going on here? The problem, in my opinion, is that programs have paths between things. I have one place, like in the user code, where someone wants to do something. And I have another place, perhaps very far away like in a driver, where that thing has to happen. If we follow the style of classes for everything with small virtual functions that do one thing, we require that _the entire path_ between the two be _widened_ to include every possible operation the system needs to do. But why? Why are we duplicating these functions everywhere? Why not just make the path narrow, and use enums so that anyone who _wants_ to take action on the specific operation can, but those who don't want to don't have to?

Another way of saying this is that type-based polymorphism, despite its promises, actually _multiplies_ every path by the operation count, everywhere in the system, for no benefit. Instead of taking the entire path from the application to the device driver and making that _entire_ path "wide" in the sense that every function has an implementation at every stage, why not just use enums to collapse that pipeline into a single function the entire way, until you _actually_ need to fan it out?

Another way to say this is, if you can do multiple things with the same pattern, why not do them? What is the _benefit_ of multiplying the code?

It's worth mentioning at this point, as an aside, that I am not so much criticizing OOP in general here as I am its modern interpretation (as it is commonly practiced today and as it appears in Java, C++, and in "Clean Code" as well). I think the design I described with the enum would be _at least somewhat familiar_ to an early OOP programmer or advocate like Alan Kay, because it is _more like message passing_ which is what they were all about. And while I don't really think it benefits programmers to think in terms of collections of objects passing messages, I am less critical of _that_ idea than the virtual methods idea, and I do use things that look like message passing fairly often (as I did in this case with loadable drivers).

Now, in _most_ systems, I would _also_ be looking for ways to collapse across device types, too. But in this case, because we are specifically talking about a system where we say a priori that we want them to be loaded separately, then the only place we can do that is _inside_ the device driver, and we're not really talking about that part of the architecture. But if we were, I would be looking for those opportunities. So if, for example, several devices had similar interfaces, that only differed by a few things here and there, I would do _another_ non-"Clean Code" thing which is to collapse those into a single driver with "if's" based on the device types where appropriate.

I'll stop there, since I mentioned a lot of things, but hopefully that gives the general idea. I use this same approach basically everywhere - anything that looks similar gets collapsed into one thing, with an enum or a flags field that differentiates it. And it tends to produce the polar opposite of "Clean Code"-style code, because that style typically does the opposite: it creates the _maximum_ number of types and functions, whereas I am trying to produce a much smaller number - perhaps not the _minimum_, but certainly much less than Clean Code.

---

**Bob**:
OK, I think I see where you are going with this.  So first let me say: "Sure, looks good to me."  The bullet points you added after the fact are all quite valid and the design you picked works well in that case.

In the first point you assume that operations will increase beyond the two originally proposed.  And as we both agreed before, and as I wrote in _Clean Code_ (which, by the way, is not the same as your "Clean Code") when operations proliferate more rapidly than types, switch statements are better.

In point two and three you raise the specter of muti-threading.  You are, of course, correct that queuing up operations is a lot easier if you use request packets of the type you designed.  No argument there.

The last point proposed a kind of "hook" for unknown and unspecified possibilities in the future.  OK, if you think those unknown and unspecified possibilities are likely then you have to consider them early; but that raises a number of other concerns that we should likely not address in this document.  So I'll let that point pass.

So now, where are we?

You posed a solution that uses dynamic polymorphism to select between device types, and then a switch statement to select operations.  I have no problem with this.  It works well, and satisfies my concerns about dependency inversion and programmer cycles.  I will say, however, that it is ironic that after your video, and after all the stress that has been put on saving machine cycles, you eventually chose a design that sacrifices machine cycles to save programmer cycles.   After all, on the OS side you've got to package up that request packet, hand it to the dynamically dispatched handler, and then run the operation ID through a switch.  ;-)

And so I think we wind up in the same place.  When operations proliferate more rapidly than types we both use switches.  When types proliferate more rapidly than operations we both use dynamic dispatch.  We are both willing to sacrifice machine cycles to save programmer cycles.

We are two individuals on the same island, the only difference being that I wear a shirt emblazoned with _Clean Code_, and you wear one emblazoned with "Clean Code".

Thank you for a stimulating debate.  I appreciate the candor, and the civility that you exercized _here_, if not everywhere else.  I have come to respect your experience, your knowledge, and your articulation.  Writing is hard, and you appear to have mastered the art along with the art of software.  I hope in future we can do this again.  And if you ever want a recommendation to a publisher, let me know.

---

**CASEY**: Well, I disagree with most of that, but if we're ending it here, I'll just add my final responses for github posterity:

* Regarding _"as I wrote in _Clean Code_ (which, by the way, is not the same as your "Clean Code"),"_ well, the point of this discussion was for you to elaborate on what is not the same. But your design for this IO system looks exactly like my "Clean Code" example - a virtual function for every operation, and one class per element in the system with no predication. So what are these differences that you're referring to? Now would be the time to explain what they are, since that was the point of the concrete example. If this is a bad example for illustrating the differences, that's fine, but it was the first one you gave so I assumed it would be one you'd want to use.

* Regarding _"when operations proliferate more rapidly than types, switch statements are better,"_ that was not the case here. In no way are operations proliferating more rapidly than types in this system. Vendors will add drivers to an OS constantly, perhaps monthly or even weekly, whereas the number of operations in a particular system tends to go up much more slowly (once every few months at maximum, but more like once a year for something like an IO subsystem). It is the opposite of what you said. This is an important distinction, because what I am demonstrating here is the _opposite_ of your rule: this is showing that _even in the case where types proliferate far more rapidly than operations_, as is the case with drivers in an OS, the principle doesn't work. Enums are better _in both cases_. _Specifically because_ you have potentially thousands of types in the system (all the different drivers all the vendors have ever shipped), adding a single operation, however rarely, can cost _massive_ programmer cycles due to the unnecessary work multiplication across types that vtables cause. Another way to say this would be, enums are _more_ important in a system where types proliferate rapidly, _not less_.

* Regarding _"you eventually chose a design that sacrifices machine cycles to save programmer cycles,"_ I did no such thing. This design achieves both, that's why I like it. It is _dramatically_ faster to use something like a packet-based system than something like your original proposed design, because you do not take a ring transition on every operation. New OS IO APIs are now all designed this way: the user writes data without talking to the OS, and a kernel thread picks up those data writes. Nobody ever makes a function call, except occasionally to ensure the kernel thread hasn't gone to sleep :)  This is what I meant by the bullet point, "If at some point we decide that users should be able to do multithreaded/bulk IO ops" I am talking about the necessity that _actually occurred_ in both Linux and Windows of removing the frequency of ring transitions _for saving CPU cycles_. None of this is trading CPU cycles for programmer cycles. It's achieving both. The Linux kernel design of io_uring looks like my design! They did not add that to save programmer cycles. They added it because they wanted the highest possible IO throughput. This is an almost universal principle of modern OS design: anything that can be turned into data writes should be, and function calls should be minimized. It's been true for GPUs, for NICs, and for our example, disk IO.

* Regarding _"And so I think we wind up in the same place.  When operations proliferate more rapidly than types we both use switches.  When types proliferate more rapidly than operations we both use dynamic dispatch,"_ Again, I don't see how you got there. Obviously types are proliferating more rapidly in this system, so that part is not true. If we don't believe drivers are proliferating rapidly, why are we loading them dynamically? I thought that was the entire point of the example! But perhaps more importantly, we are not "using dynamic dispatch" here in the way you seem to be suggesting. As I said when I posted the proposed design, I would _also_ do this inside the drivers themselves. I _would not_ duplicate drivers to remove if statements and switches inside a driver that allowed that driver to handle multiple similar devices. The only reason there are function pointers in this system is because the problem definition required that we load the driver from a different module, and we are not presuming a JIT or something which can weld things together for us. That introduces a mandatory cut, so we cannot get rid of it because the problem is defined to contain it.

But note that that is _not_ the same between our two approaches. I have the function pointer there because it's required. And you'll note I minimized the number all the way down to one. I didn't put it in there because I think it saves programmer time. In fact, I'm not really sure I want it there at all. I haven't actually implemented this particular system in an OS, so it was somewhat off the top of my head, but _it's very possible_ that if I actually went to write this, I wouldn't actually include that function pointer at all. Instead, I might just have the OS thread reading the queue and prefiltering the packets for quota/permissions, then updating a shared memory address that lets the driver know it can process the packets directly. Without actually implementing, I can't say that's for sure what I would do, but, it's probably something I would try.

So it feels like you're overstating the similarity of our approaches, but, if you think they're that similar, then, I guess that's just where we end up! Thanks for taking the time to create this thread which pushed the github emoji checker well beyond its limits.

---

**Bob**: Thank you Casey.  I believe we should let our disagreement stand at this point and let our audience be the final judge.  This has been fun.  Perhaps we'll do it again some day.

```
typedef void raw_device_handler(u32 Instance, raw_device_request *Packet, raw_device_result *Result);
struct raw_device
{
	raw_device_handler *Handler;
};
```

instead of the class version. But the only real reason I have to write that is, of course, because C++ is crappy and you can't say "I would like this class to contain function pointers, instead of a pointer to a table of function pointers, please." Otherwise, I wouldn't have a problem with the class version. So, although I would rather have the direct pointer version in practice, for purposes of this discussion (since we only care about programmer cycles right now), you can ignore my raw_device struct and we can just assume that it is written in class form like yours since I can't think of any programmer-cycles benefit to my version in this case:

```
class raw_device
{
public:
	void Handler(raw_device_request *Packet, raw_device_result *Result);
};
```

Since the user never sees a raw_device - they would just be using the 32-bit raw_device_id handle - leaving this as a class is fine.

When someone implements a device driver, they really just implement one function:

```
void raw_device::Handler(raw_device_request *Packet, raw_device_result *Result)
{
	switch(Packet->Op)
	{
		case RIO_read:
		// etc.

		case RIO_write:
		// etc.

		case RIO_get_name:
		// etc.

		default:
		// write error Result
	}
}
```

Obviously, the user-side version of this can be the same as yours:

```
int read(char* name, size_t offset, size_t n, char* buf);
int write(char* name, size_t offset, size_t n, char* buf);
```

However, I might recommend that it be more like:

```
raw_device_id lookup_device(char* name);
int read(raw_device_id device, size_t offset, size_t n, char* buf);
int write(raw_device_id device, size_t offset, size_t n, char* buf);
```

in _either_ case, but that is largely irrelevant, because that's solely about the lookup to map to a specific device, and can be used with either design.

Anyway, over the course of the development of the OS, I think this implementation _saves_ programmer cycles, potentially a lot of them, compared to the one I understand to be favored by the "Clean Code" method (which is also the one you wrote above). Again, in my understanding, the reason that yours looks the way it looks is specifically because of two design ideas that you mention often in books and in lectures:

* You favor having one class for each type of thing (in this case a driver), with one virtual member function per operation that the thing does (in this case read, write, and get_name)
* Operation permutations are not supposed to pass through functions - so passing an enum to a function that says what to do is bad, you should have one function per thing that gets done. I've seen this mentioned multiple times - not just for switch statements, but also when you say functions with "if" statements on function parameters in them, that modifies what the function does, you're supposed to rewrite that as two functions.

In my experience, these two things cost a lot of programmer cycles. I prefer to collapse things into functions with parameters when possible, because it reduces the total number of things that the programmer has to think about, and reduces the number of files, lines of code, and data permutations they have to consider. It allows functions to "pass through" information about what is happening without knowing what it is. Again, I tend to think of this as "operation-primal", because instead of focusing on making your _types_ polymorphic, you're making your _functions_ polymorphic.

I don't know if "abstracting file IO" is the best example to pick, but it was the first one you brought up, and it _happens_ to contrast the two designs enough in my opinion, so it works for me. Here's why I think an enum-based design saves the programmer(s) cycles:

* In most systems, we do not know all the functions that we are going to do ahead of time. When operating across a hard boundary like a driver, using operation codes instead of virtual function calls allows us to add functions dynamically without recompiling all of our drivers. If we want, for example, to add a new function like "trim" (such as what happened with real IO protocols when SSDs happened), if we are just passing opcodes to a driver, we don't have to recompile _anything_. All the old drivers just ignore the code, whereas the new drivers act on it, which is what we want. In the class case, either all the drivers have to be recompiled, or we have to write a utility class which stubs the new function, and wrap all the old drivers in that utility class. If we don't do this, the vtables for the old drivers will all be the wrong size, so we can't use them.
* In any modern system, multithreading is a concern, but this is especially true for an operating system. Having the protocol be structure-based, with an operation code, allows us to trivially buffer operations in things like io rings or other intermediaries _without writing any new code_. The entire system remains identical, and we need only add the new buffer logic. In the class system, we don't have any way to represent function operations with data, so wse must _introduce_ a data format for storing the calls - which of course will end up looking basically like the code for the non-class version, so you end up having to write my version _as well as_ the class version. This, by the way, seems to happen in almost _all_ OOP systems I see, because eventually they need to serialize or something similar, and so they have to write my version _as well as_ their version, but they don't seem to realize how much time they're wasting this way!
* If at some point we decide that users should be able to do multithreaded/bulk IO ops, with the enum version there are _zero_ changes to the internals and the drivers. All we do is add whatever user-land interface we want for this (presumably a circular buffer they write to), and everything "just works", with no translation necessary because the internals already worked on data instead of vtables/function calls anyway.
* If we would like to allow 3rd parties to have private communication channels for their devices, where user-level code can do IO ops that are only defined on those devices, there is no way to do this if we stick with the idea that each operation has to be represented by its own virtual function. It requires a complete side-channel implementation that has to be created specially and deployed by each vendor separately, because we have no idea how many functions each device will need or what they will be called. With the enum version, it's trivial. Just reserve half the opcode range for private stuff, and let anything in that range pass through to the device. Now the vendor ships an enum table to the end users, or an SDK with function calls that write those enums for them if you prefer, and that's it. Whether or not you'd actually _want_ to do this in an OS is questionable, because you're trusting the driver more here, but the point is the architecture _makes it trivial_. Whether or not you should allow it on security grounds is another question, and may depend on the type of OS.

Also, I should mention, that all of those things I just listed were _actual_ things that happened to IO subsystems in OS design. So none of those are hypothetical things that don't usually occur, or which are cherry-picked to be beneficial to enum-based design. Those are basically just _the exact things that have happened_ over the past ~20 years of OS IO subsystem evolution in a broad sense.

So what is really going on here? The problem, in my opinion, is that programs have paths between things. I have one place, like in the user code, where someone wants to do something. And I have another place, perhaps very far away like in a driver, where that thing has to happen. If we follow the style of classes for everything with small virtual functions that do one thing, we require that _the entire path_ between the two be _widened_ to include every possible operation the system needs to do. But why? Why are we duplicating these functions everywhere? Why not just make the path narrow, and use enums so that anyone who _wants_ to take action on the specific operation can, but those who don't want to don't have to?

Another way of saying this is that type-based polymorphism, despite its promises, actually _multiplies_ every path by the operation count, everywhere in the system, for no benefit. Instead of taking the entire path from the application to the device driver and making that _entire_ path "wide" in the sense that every function has an implementation at every stage, why not just use enums to collapse that pipeline into a single function the entire way, until you _actually_ need to fan it out?

Another way to say this is, if you can do multiple things with the same pattern, why not do them? What is the _benefit_ of multiplying the code?

It's worth mentioning at this point, as an aside, that I am not so much criticizing OOP in general here as I am its modern interpretation (as it is commonly practiced today and as it appears in Java, C++, and in "Clean Code" as well). I think the design I described with the enum would be _at least somewhat familiar_ to an early OOP programmer or advocate like Alan Kay, because it is _more like message passing_ which is what they were all about. And while I don't really think it benefits programmers to think in terms of collections of objects passing messages, I am less critical of _that_ idea than the virtual methods idea, and I do use things that look like message passing fairly often (as I did in this case with loadable drivers).

Now, in _most_ systems, I would _also_ be looking for ways to collapse across device types, too. But in this case, because we are specifically talking about a system where we say a priori that we want them to be loaded separately, then the only place we can do that is _inside_ the device driver, and we're not really talking about that part of the architecture. But if we were, I would be looking for those opportunities. So if, for example, several devices had similar interfaces, that only differed by a few things here and there, I would do _another_ non-"Clean Code" thing which is to collapse those into a single driver with "if's" based on the device types where appropriate.

I'll stop there, since I mentioned a lot of things, but hopefully that gives the general idea. I use this same approach basically everywhere - anything that looks similar gets collapsed into one thing, with an enum or a flags field that differentiates it. And it tends to produce the polar opposite of "Clean Code"-style code, because that style typically does the opposite: it creates the _maximum_ number of types and functions, whereas I am trying to produce a much smaller number - perhaps not the _minimum_, but certainly much less than Clean Code.

---

**Bob**:
OK, I think I see where you are going with this.  So first let me say: "Sure, looks good to me."  The bullet points you added after the fact are all quite valid and the design you picked works well in that case.

In the first point you assume that operations will increase beyond the two originally proposed.  And as we both agreed before, and as I wrote in _Clean Code_ (which, by the way, is not the same as your "Clean Code") when operations proliferate more rapidly than types, switch statements are better.

In point two and three you raise the specter of muti-threading.  You are, of course, correct that queuing up operations is a lot easier if you use request packets of the type you designed.  No argument there.

The last point proposed a kind of "hook" for unknown and unspecified possibilities in the future.  OK, if you think those unknown and unspecified possibilities are likely then you have to consider them early; but that raises a number of other concerns that we should likely not address in this document.  So I'll let that point pass.

So now, where are we?

You posed a solution that uses dynamic polymorphism to select between device types, and then a switch statement to select operations.  I have no problem with this.  It works well, and satisfies my concerns about dependency inversion and programmer cycles.  I will say, however, that it is ironic that after your video, and after all the stress that has been put on saving machine cycles, you eventually chose a design that sacrifices machine cycles to save programmer cycles.   After all, on the OS side you've got to package up that request packet, hand it to the dynamically dispatched handler, and then run the operation ID through a switch.  ;-)

And so I think we wind up in the same place.  When operations proliferate more rapidly than types we both use switches.  When types proliferate more rapidly than operations we both use dynamic dispatch.  We are both willing to sacrifice machine cycles to save programmer cycles.

We are two individuals on the same island, the only difference being that I wear a shirt emblazoned with _Clean Code_, and you wear one emblazoned with "Clean Code".

Thank you for a stimulating debate.  I appreciate the candor, and the civility that you exercized _here_, if not everywhere else.  I have come to respect your experience, your knowledge, and your articulation.  Writing is hard, and you appear to have mastered the art along with the art of software.  I hope in future we can do this again.  And if you ever want a recommendation to a publisher, let me know.

---

**CASEY**: Well, I disagree with most of that, but if we're ending it here, I'll just add my final responses for github posterity:

* Regarding _"as I wrote in _Clean Code_ (which, by the way, is not the same as your "Clean Code"),"_ well, the point of this discussion was for you to elaborate on what is not the same. But your design for this IO system looks exactly like my "Clean Code" example - a virtual function for every operation, and one class per element in the system with no predication. So what are these differences that you're referring to? Now would be the time to explain what they are, since that was the point of the concrete example. If this is a bad example for illustrating the differences, that's fine, but it was the first one you gave so I assumed it would be one you'd want to use.

* Regarding _"when operations proliferate more rapidly than types, switch statements are better,"_ that was not the case here. In no way are operations proliferating more rapidly than types in this system. Vendors will add drivers to an OS constantly, perhaps monthly or even weekly, whereas the number of operations in a particular system tends to go up much more slowly (once every few months at maximum, but more like once a year for something like an IO subsystem). It is the opposite of what you said. This is an important distinction, because what I am demonstrating here is the _opposite_ of your rule: this is showing that _even in the case where types proliferate far more rapidly than operations_, as is the case with drivers in an OS, the principle doesn't work. Enums are better _in both cases_. _Specifically because_ you have potentially thousands of types in the system (all the different drivers all the vendors have ever shipped), adding a single operation, however rarely, can cost _massive_ programmer cycles due to the unnecessary work multiplication across types that vtables cause. Another way to say this would be, enums are _more_ important in a system where types proliferate rapidly, _not less_.

* Regarding _"you eventually chose a design that sacrifices machine cycles to save programmer cycles,"_ I did no such thing. This design achieves both, that's why I like it. It is _dramatically_ faster to use something like a packet-based system than something like your original proposed design, because you do not take a ring transition on every operation. New OS IO APIs are now all designed this way: the user writes data without talking to the OS, and a kernel thread picks up those data writes. Nobody ever makes a function call, except occasionally to ensure the kernel thread hasn't gone to sleep :)  This is what I meant by the bullet point, "If at some point we decide that users should be able to do multithreaded/bulk IO ops" I am talking about the necessity that _actually occurred_ in both Linux and Windows of removing the frequency of ring transitions _for saving CPU cycles_. None of this is trading CPU cycles for programmer cycles. It's achieving both. The Linux kernel design of io_uring looks like my design! They did not add that to save programmer cycles. They added it because they wanted the highest possible IO throughput. This is an almost universal principle of modern OS design: anything that can be turned into data writes should be, and function calls should be minimized. It's been true for GPUs, for NICs, and for our example, disk IO.

* Regarding _"And so I think we wind up in the same place.  When operations proliferate more rapidly than types we both use switches.  When types proliferate more rapidly than operations we both use dynamic dispatch,"_ Again, I don't see how you got there. Obviously types are proliferating more rapidly in this system, so that part is not true. If we don't believe drivers are proliferating rapidly, why are we loading them dynamically? I thought that was the entire point of the example! But perhaps more importantly, we are not "using dynamic dispatch" here in the way you seem to be suggesting. As I said when I posted the proposed design, I would _also_ do this inside the drivers themselves. I _would not_ duplicate drivers to remove if statements and switches inside a driver that allowed that driver to handle multiple similar devices. The only reason there are function pointers in this system is because the problem definition required that we load the driver from a different module, and we are not presuming a JIT or something which can weld things together for us. That introduces a mandatory cut, so we cannot get rid of it because the problem is defined to contain it.

But note that that is _not_ the same between our two approaches. I have the function pointer there because it's required. And you'll note I minimized the number all the way down to one. I didn't put it in there because I think it saves programmer time. In fact, I'm not really sure I want it there at all. I haven't actually implemented this particular system in an OS, so it was somewhat off the top of my head, but _it's very possible_ that if I actually went to write this, I wouldn't actually include that function pointer at all. Instead, I might just have the OS thread reading the queue and prefiltering the packets for quota/permissions, then updating a shared memory address that lets the driver know it can process the packets directly. Without actually implementing, I can't say that's for sure what I would do, but, it's probably something I would try.

So it feels like you're overstating the similarity of our approaches, but, if you think they're that similar, then, I guess that's just where we end up! Thanks for taking the time to create this thread which pushed the github emoji checker well beyond its limits.

---

**Bob**: Thank you Casey.  I believe we should let our disagreement stand at this point and let our audience be the final judge.  This has been fun.  Perhaps we'll do it again some day.