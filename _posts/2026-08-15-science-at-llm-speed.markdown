---
layout: post
title: "Science at LLM Speed"
subtitle: "Research-like output is cheap. A valid claim still needs evidence that can show it is wrong."
date: 2026-08-15 14:00:00 -0400
group: ai
series: lab
permalink: /series/the-machine-in-the-lab/science-at-llm-speed/
categories: ai research zabriskie agents
---

*This is Part 1 of [The Machine in the Lab](/series/the-machine-in-the-lab/), a seven-part series about building SetScope, a live Goose song guesser, with an autonomous research program built from large language models (LLMs).*

Earlier this year, the leaders of one of the largest machine-learning conferences found references in submitted papers to publications that did not exist.

This was not a thought experiment about what a language model might do. It was not a benchmark, a standardized test, in which researchers asked a model to write a literature review and then counted the invented citations. These were papers submitted to the 2026 International Conference on Learning Representations (ICLR), accompanied by bibliographies that were supposed to describe prior work. The conference built a screening system, sent the flagged references to senior reviewers, and then had the conference leaders check them again. Every paper with a confirmed hallucinated reference was desk rejected, meaning it was rejected before normal peer review.

The most interesting part of the [conference leaders' account](https://blog.iclr.cc/2026/03/31/a-retrospective-on-the-iclr-2026-review-process/) is how much work it took to establish that a paper in a bibliography was not a paper. The automated system incorrectly flagged some real references. Translated titles looked suspicious. At least three people reviewed every confirmed case. The conference did not trust one detector to decide whether a citation existed.

Whatever process produced those bibliographies did not catch the error.

This is a strange failure because background research is one of the activities these systems appear to make almost effortless. Ask for the important papers in an area and a model will produce authors, titles, dates, summaries, and a tidy account of how the work fits together. The answer has the shape of a literature review before anyone has done the work of reviewing the literature.

Sometimes the papers are real. Sometimes a real paper is attached to a claim it does not support. Sometimes the title, authors, or venue are slightly wrong. And sometimes the entire reference was generated because a plausible citation completed the paragraph.

All four cases look approximately the same on the page.

## A plausible bibliography can describe papers that never existed

We knew this behavior existed before it began appearing in conference submissions. In 2023, William H. Walters and Esther Isabelle Wilder asked OpenAI's GPT-3.5 and GPT-4 language models to produce short literature reviews across 42 topics. GPT stands for Generative Pre-trained Transformer. They checked 636 generated citations. In their [controlled study](https://www.nature.com/articles/s41598-023-41032-5), 55 percent of the GPT-3.5 citations and 18 percent of the GPT-4 citations referred to works the researchers could not verify as existing. Many citations to real work also contained substantial bibliographic errors.

Those numbers do not tell us what fraction of references in current scholarship were written by a model. They describe particular models, prompts, and topics in a controlled experiment. A broken citation in a paper does not contain a record of the tool that produced it. People invented, mangled, and copied references long before ChatGPT.

But the ICLR cases establish the part that matters here: nonexistent references made it through whatever process their authors used to produce a submission for a major research conference. The scholarly form was present. The scholarship that the form purported to reference was not.

The obvious response is to connect the model to a search system. OpenScholar, for example, searches a collection of 45 million open-access papers, retrieves relevant passages, generates an answer with citations, and then checks the answer against those sources. In the authors' [evaluation](https://www.nature.com/articles/s41586-025-10072-4), the system substantially improved the citation accuracy and correctness of a general-purpose model on scientific synthesis tasks.

This is better, but the reason it is better matters. The model did not become more trustworthy by writing a more confident explanation of its sources. The system changed the task. Candidate claims had to be attached to retrieved passages. Citations could be checked against documents that existed outside the generated answer. Expert readers could inspect whether the source supported the sentence.

Fetching the sources does not make a literature review true. A real paper can still be misunderstood, cited out of context, or asked to carry a larger claim than its experiment supports. What the sources supply is the possibility of contradiction. There is now something outside the paragraph that can show the paragraph is wrong.

That difference between fluent support and evidence capable of contradiction is the problem I kept encountering in my own work.

## The research loop gets faster

The citation problem is unusually easy to see because existence is a fairly crisp property. Either the referenced document can be found or it cannot. Most research decisions are not like that.

A model can now help formulate a question, search for related work, write experiment code, select a statistical test, produce a figure, explain the figure, draft the limitations, and review the finished manuscript. Each output can become the input to the next step. A single person can move through activities that once required more time, more specialized assistance, or both.

I wanted to use that capability to build an automatic live song guesser. I run [Zabriskie](https://zabriskie.app), a community for live-music fans. During a livestream by the band [Goose](https://www.goosetheband.com), viewers who want a running setlist have to wait for a person or an external service to recognize each song and enter it. I wanted Zabriskie to listen to the stream, identify the current song, and post the guess while the show was still happening. That product became SetScope.

This is a different problem from [Shazam](https://swh.princeton.edu/~cuff/ele201/files/Wang03-shazam.pdf). Shazam fingerprints a short excerpt and looks for the same recording in its database. SetScope hears a performance that has never existed before. Goose might play the song faster or slower, change the key or arrangement, stretch the opening, or move from the written song into a long improvisation. The system cannot search for an identical recording. It has to recognize the musical identity that survives those changes.

I also wanted artificial intelligence (AI) to do the research needed to build it. I would give the system recordings paired with their correct song titles and tell it what SetScope needed to do. It would come up with ideas for recognizing songs, write the code, use those examples to train models, run tests, examine the mistakes, and decide what to try next. I would not approve every step. That was the human-out-of-the-loop part of the project.

The implementation speed is what made that plan plausible for one person. During my doctoral degree (PhD), I learned how much of a systems experiment happens before the experiment. [Filibuster](/publications/filibuster-socc-2021.pdf), the testing framework I was building for applications spread across multiple services and computers, could not evaluate an existing application until that application had been adapted to run through software hooks that let Filibuster observe and alter its behavior. Building the prototype took me three months of full-time engineering. It used [OpenTelemetry](https://opentelemetry.io/), a standard for collecting information about what happens as a request moves through an application.

That only got the application to the starting line. Filibuster works by deliberately introducing failures while an application's existing tests run. If the target application did not already have useful tests, I had to write them before the framework had anything meaningful to explore. For one production application used in an evaluation paper, that took another six months.

With the agents I use now, I believe I could produce a first implementation of many of those software hooks and supporting tests in days, perhaps a single day. During the SetScope work, agents built audio scanners, catalogs of recordings and their source history, programs for measuring audio and comparing song recognizers, and detailed logging inside the live browser on that timescale.

None of those components is a controlled equivalent of the OpenTelemetry work, and I cannot rerun my PhD for comparison. The estimate describes what I believe would have happened, not a measured speedup. Code arriving quickly would not establish that the added software hooks preserved the application's behavior or that the generated tests supported the evaluation. Those questions would still require evidence. But even compressing the first implementation from months into days changes which research projects one person can plausibly attempt.

While I was writing this, Jeff Dean, Sanjay Ghemawat, Oriol Vinyals, and Quoc Le left Google to form a company called [Discovery Loop](https://www.axios.com/2026/08/06/googles-ai-leadership-shuffle). Public descriptions say its goal is to [automate experimental loops](https://www.itpro.com/business/leadership/deepmind-ceo-demis-hassabis-steps-aside-amid-google-leadership-shake-up) in science and engineering: propose an experiment, run it, evaluate what happened, and decide what to try next.

That is the same kind of autonomous researcher I was trying to use to build SetScope. Their target is science and engineering broadly. Mine was Goose song identification.

When a system can choose and run the next experiment by itself, a bad result can do more than produce one wrong answer. It can change what the system tries next. Examples meant to remain unseen can enter development and alter the next hypothesis. A convenient stand-in for the real question can quietly replace the question. A test of one part of a product can become a claim about a product that never ran.

## A polished analysis is not necessarily a scientific result

Autonomous research is the most ambitious version of the problem, but a smaller version is already common. LLMs have made it cheaper to produce something that looks like the output of a research process: a paper, methodology page, interactive analysis, benchmark, data product, or long post with equations and charts.

This can be genuinely useful. In a participatory study of 15 people performing generative-AI-assisted data analysis, [Drosos and colleagues](https://doi.org/10.1145/3663384.3663389) observed participants using a model for information gathering, hypothesis generation, and analysis strategy. The same participants described verification as effortful and time-consuming.

Useful analysis does not have to occur inside a university or become a paper. A fan project can rank performances, organize an archive, publish its formulas, and offer an excellent discovery tool. But if it also claims that the score reveals a property of improvisation, then the human judgments used as correct answers, the unit being measured, and their relationship to that musical property become part of the claim. Equations, transparent code, and polished charts do not settle whether the evidence is adequate.

This problem existed before LLMs. What changes is the cost of producing the complete package. Code, prose, caveats, visualizations, and a memorable result can now arrive together, quickly enough that their coherence feels like evidence that the empirical work occurred.

My own project produced exactly that kind of convincing package, more than once. The form did not create the errors. It made them harder to notice.

## The reviewer might be a model too

One possible answer is review. Authors produce work quickly; reviewers slow it down, inspect the assumptions, and require the claims to survive contact with another person. Except the same systems have entered that loop.

A 2024 study published at the International Conference on Machine Learning examined reviews from four major machine-learning conferences, including ICLR. Its [estimate across the full collection](https://proceedings.mlr.press/v235/liang24b.html) was that 6.5 to 16.9 percent of review text had been substantially modified or produced by language models, beyond minor writing assistance. That does not identify any particular review as machine-written. It does show that the tool was helping reviewers respond to claims as well as helping authors present them.

There are constructive versions. In a [randomized study](https://doi.org/10.1038/s42256-026-01188-x) at ICLR 2025, some reviewers received model-generated suggestions about vague language, possible misunderstandings, and unprofessional comments. Some revised their reports, and evaluators who did not know which reviews received assistance rated the revisions as more informative.

That is evidence that a model can improve one part of a human review process, not that it can replace peer review. A second model does not become independent review merely by being a second model. Two systems can share training data, conventions, blind spots, and a preference for the same fluent explanation. Adding agents changes the number of outputs. It does not necessarily change the source of judgment.

Human review is not a magical external check either. Reviewers miss errors, disagree, rush, and reward familiar methods. The question is what new information and incentives each check adds. If every stage evaluates the same generated output in roughly the same way, the process can become impressively self-consistent without becoming more correct.

## The machine can find something real

There is an easy version of this essay in which every section supplies another example of AI making science worse. It would also be wrong.

FunSearch used a language model to generate candidate programs for mathematical problems. The system executed them, scored them with a test supplied by the researchers, retained the strongest programs, and used those programs to guide further search. The resulting [Nature paper](https://www.nature.com/articles/s41586-023-06924-6) reported new solutions to a problem about arranging finite sets and useful strategies for packing items into a limited number of containers.

The model was valuable because it could search a space of programs productively. It was not asked to decide, in prose, that its own program was interesting. The programs ran, the evaluator scored them, and other people could inspect the result.

[AI Scientist-v2](https://arxiv.org/abs/2504.08066) operated under a weaker judge. It generated machine-learning manuscripts end to end, with humans choosing initial ideas and selecting the best completed run. One manuscript scored above the acceptance threshold at an ICLR workshop. The authors' inspection also found missing citations, examples that may have influenced both development and evaluation, incorrect figure interpretations, and unused code for a statistical technique intended to make confidence scores match observed probabilities.

These checks do not provide the same evidence. A program evaluator can reject a candidate against a specified property. An experiment can contradict a prediction. A source can fail to support a sentence. Human review adds judgment, but a reviewer can still be persuaded by the same polished explanation as everyone else.

The system becomes more useful when generated candidates encounter information that was not produced by the same act of generation and can return an unwelcome answer.

Before treating a check as independent, I now ask three questions: What information does it add that was unavailable to the process that generated the result? What unfavorable answer can it return? Which precise claim would that answer reject?

## Then my own research loop failed twice

I then changed the task. Instead of improving SetScope's song guesses, I asked the system to use the recordings to study improvisation. I had years of recordings paired with song titles and other annotations, ways to measure rhythm, harmony, and texture, models that turn audio into numbers that can be compared, and an LLM agent that could scan recordings, organize their source history, calculate those measurements, train programs to classify them, run evaluations, analyze failures, and modify the live application. What had been an implausibly large solo project looked tractable.

The recordings seemed to offer a way to investigate harder questions about improvisation: whether measurements of the music could identify when a performance left its composed structure, whether different forms of jamming produced distinct patterns in those measurements, and whether the same patterns appeared across performances. Those questions were more interesting than simple song identification. They were also much harder to define.

I am a PhD-trained systems researcher. I know what a holdout is: a final test set kept unseen while a system is developed. I also know why an experimental method has to match its implementation. That was not enough. The first research notebook, a running workspace of code, data, experiments, and reports, expanded from song recognition into several improvisation questions without preserving one final test capable of evaluating the path the system chose.

The audit found several problems, not one neat leak. Most decisively, a detector for Type II improvisation, where the band leaves a song's composed structure for open-ended playing, failed on 44 recordings that had not been used to develop it. I had described the detector on a research webpage and shown it to two friends. It mistook changes inside composed songs for improvisation. I took the webpage down and started again.

For the restart, I permanently assigned many of the same recordings to three jobs: examples the models could learn from, examples used to choose among competing methods, and a final test that neither process was supposed to see. I also wrote a new method that explicitly warned against fixed ninety-second jam boundaries. The first implementation reused them anyway. The research loop never stopped to report that the code contradicted the declared method. Working from those reports, I put two draft research posts online at their direct web addresses and sent friends listening assignments built from the analysis.

The consequences were larger than the lines of code that caused them. I was responsible for putting the work online and involving other people's time. But the lesson could not be that I should manually reconstruct every assignment of recordings and trace every fixed numerical value before accepting any result. A system that requires that level of supervision is not running the research process autonomously. It is generating work for a human auditor. The system needed to prevent violations of its rules or report them when they occurred. It had done neither. I deleted the second notebook, took the drafts down, and abandoned the listening study.

We eventually gave the system its original job again: improve the live song guesser. On August 13, the running SetScope product proposed the correct identity at least once for 10 of 12 recorded song performances while the band was playing. It also missed songs, switched guesses at the wrong time, and exposed failures in the live audio path.

This was a product field test, not a formal whole-show accuracy estimate, and the complete operational record belongs later in this series. Neither audio from that performance nor its completed setlist existed during development. The surviving record shows what reached SetScope's decision logic, not confirmed viewer-visible delivery.

The project would not exist at its current scale without LLMs. The failures taught me that speed changes the location of the work. Producing the next output becomes cheap. Establishing what the output means, what information entered it, and what could show it is wrong does not.

The machine can participate in research. The harder problem is deciding what it may do without asking, what evidence must survive each iteration, and what can stop a bad result before it becomes the premise of the next experiment. This series is about learning to build those constraints after discovering, repeatedly, that a persuasive report was not one of them.

## What Comes Next

Part 2 reconstructs the two notebooks I deleted. Part 3 follows the holdout boundary we repeatedly crossed and the record needed to know which evidence each later version had already used.

Part 4 shows how a reviewed experiment can pass every check and still misinterpret the music. Part 5 moves the test into the browser, where capture, controller behavior, and the rendered result become part of the experiment.

Part 6 describes the constraints we built around the research loop. Part 7 returns after Goose's August run with the versions that actually ran, their guesses, their failures, and the repairs made between shows.

Next: two notebooks that contained weeks of analysis and almost no result I could still defend.
