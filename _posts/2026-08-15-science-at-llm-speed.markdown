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

Earlier this year, the program chairs for one of the largest machine-learning conferences found references in submitted papers to publications that did not exist.

This was not a thought experiment about what a language model might do. It was not a benchmark in which researchers asked a model to write a literature review and then counted the invented citations. These were papers submitted to ICLR 2026, accompanied by bibliographies that were supposed to describe prior work. The conference built a screening system, sent the flagged references to area chairs, and then had the program chairs check them again. Every paper with a confirmed hallucinated reference was desk rejected.

The most interesting part of the [program chairs' account](https://blog.iclr.cc/2026/03/31/a-retrospective-on-the-iclr-2026-review-process/) is how much work it took to establish that a paper in a bibliography was not a paper. The automated system produced false positives. Translated titles looked suspicious. At least three people reviewed every confirmed case. The conference did not trust one detector to decide whether a citation existed.

Whatever process produced those bibliographies did not catch the error.

This is a strange failure because background research is one of the activities these systems appear to make almost effortless. Ask for the important papers in an area and a model will produce authors, titles, dates, summaries, and a tidy account of how the work fits together. The answer has the shape of a literature review before anyone has done the work of reviewing the literature.

Sometimes the papers are real. Sometimes a real paper is attached to a claim it does not support. Sometimes the title, authors, or venue are slightly wrong. And sometimes the entire reference was generated because a plausible citation completed the paragraph.

All four cases look approximately the same on the page.

## A plausible bibliography can describe papers that never existed

We knew this behavior existed before it began appearing in conference submissions. In 2023, William H. Walters and Esther Isabelle Wilder asked GPT-3.5 and GPT-4 to produce short literature reviews across 42 topics. They checked 636 generated citations. In their [controlled study](https://www.nature.com/articles/s41598-023-41032-5), 55 percent of the GPT-3.5 citations and 18 percent of the GPT-4 citations referred to works the researchers could not verify as existing. Many citations to real work also contained substantial bibliographic errors.

Those numbers do not tell us what fraction of references in current scholarship were written by a model. They describe particular models, prompts, and topics in a controlled experiment. A broken citation in a paper does not contain a record of the tool that produced it. People invented, mangled, and copied references long before ChatGPT.

But the ICLR cases establish the part that matters here: nonexistent references made it through whatever process their authors used to produce a submission for a major research conference. The scholarly form was present. The scholarship that the form purported to reference was not.

The obvious response is to connect the model to a search system. OpenScholar, for example, retrieves passages from a corpus of 45 million open-access papers, generates an answer with citations, and then refines the answer through retrieval and citation verification. In the authors' [evaluation](https://www.nature.com/articles/s41586-025-10072-4), the system substantially improved the citation accuracy and correctness of a general-purpose model on scientific synthesis tasks.

This is better, but the reason it is better matters. The model did not become more trustworthy by writing a more confident explanation of its sources. The system changed the task. Candidate claims had to be attached to retrieved passages. Citations could be checked against documents that existed outside the generated answer. Expert readers could inspect whether the source supported the sentence.

Retrieval does not make a literature review true. A real paper can still be misunderstood, cited out of context, or asked to carry a larger claim than its experiment supports. What retrieval supplies is the possibility of contradiction. There is now something outside the paragraph that can show the paragraph is wrong.

That difference between fluent support and evidence capable of contradiction is the problem I kept encountering in my own work.

## More research-like work, produced faster

The citation problem is unusually easy to see because existence is a fairly crisp property. Either the referenced document can be found or it cannot. Most research decisions are not like that.

A model can now help formulate a question, search for related work, write experiment code, select a statistical test, produce a figure, explain the figure, draft the limitations, and review the finished manuscript. Each output can become the input to the next step. A single person can move through activities that once required more time, more specialized assistance, or both.

I wanted to use that capability to build an automatic live song guesser. I run Zabriskie, a community for live-music fans. During a livestream by the band Goose, viewers who want a running setlist have to wait for a person or an external service to recognize each song and enter it. I wanted Zabriskie to listen to the stream, identify the current song, and post the guess while the show was still happening. That product became SetScope.

I also wanted AI to do the research needed to build it. I would give the system labeled recordings and tell it what SetScope needed to do. It would come up with ideas for recognizing songs, write the code, train models, run tests, examine the mistakes, and decide what to try next. I would not approve every step. That was the human-out-of-the-loop part of the project.

The implementation speed is what made that plan plausible for one person. During my PhD, I learned how much of a systems experiment happens before the experiment. [Filibuster](/publications/filibuster-socc-2021.pdf), the distributed-systems testing framework I was building, could not evaluate an existing application until that application had been adapted to run through its instrumentation. Building the OpenTelemetry-based prototype that made this possible took me three months of full-time engineering.

That only got the application to the starting line. Filibuster works by perturbing executions of an existing test suite. If the target application did not already have useful tests, I had to write them before the framework had anything meaningful to explore. For one production application used in an evaluation paper, that took another six months.

With the agents I use now, I believe I could produce a first implementation of much of that instrumentation and test scaffolding in days, perhaps a single day. During the SetScope work, agents built audio scanners, corpus manifests, feature pipelines, classifier harnesses, and live-browser instrumentation on that timescale.

None of those components is a controlled equivalent of the OpenTelemetry work, and I cannot rerun my PhD for comparison. The estimate is a counterfactual, not a measured speedup. Code arriving quickly would not establish that the instrumentation preserved the application's behavior or that the generated tests supported the evaluation. Those questions would still require evidence. But even compressing the first implementation from months into days changes which research projects one person can plausibly attempt.

While I was writing this, Jeff Dean, Sanjay Ghemawat, Oriol Vinyals, and Quoc Le left Google to form a company called [Discovery Loop](https://www.axios.com/2026/08/06/googles-ai-leadership-shuffle). Public descriptions say its goal is to [automate experimental loops](https://www.itpro.com/business/leadership/deepmind-ceo-demis-hassabis-steps-aside-amid-google-leadership-shake-up) in science and engineering: propose an experiment, run it, evaluate what happened, and decide what to try next.

That is the same kind of autonomous researcher I was trying to use to build SetScope. Their target is science and engineering broadly. Mine was Goose song identification.

When a system can choose and run the next experiment by itself, a bad result can do more than produce one wrong answer. It can change what the system tries next. Leaked data can alter the next hypothesis. A convenient proxy can become the objective. A component test can become a claim about a product that never ran.

## A polished analysis is not necessarily a scientific result

Autonomous research is the most ambitious version of the problem, but a smaller version is already common. LLMs have made it cheaper to produce an artifact that looks like the output of a research process. That artifact might be a paper. It might also be a methodology page, an interactive analysis, a benchmark, a data product, or a long post with equations and charts.

Some of the most interesting versions appear outside journals and conferences. A person with a question and a dataset can now get meaningful help writing data-cleaning code, selecting features, fitting models, and interpreting results. In a participatory study of 15 people performing generative-AI-assisted data analysis, [Drosos and colleagues](https://doi.org/10.1145/3663384.3663389) observed participants using a model for information gathering, hypothesis generation, and analysis strategy. Participants also described verification as effortful and time-consuming; several checked references, tested generated code or formulas in other tools, or tried to inspect every line.

Useful analysis does not have to occur inside a university or become a paper. A fan project may rank performances, organize an archive, publish its formulas, and offer an excellent discovery tool. Reproducing the score can show that the product implements its stated rule.

If the project also says that score reveals a property of improvisation, then the labels, measurement unit, and relationship between the score and that musical property become part of the claim. The publication venue does not settle whether the evidence is adequate. Neither do equations, transparent code, or polished charts.

This category existed before LLMs. What changes now is the cost of producing the complete package. Code, prose, caveats, visualizations, and a memorable result can arrive together, quickly enough that their coherence can feel like evidence that the underlying empirical work occurred.

My own project produced exactly that kind of convincing package, more than once. The form did not create the errors. It made them harder to notice.

## The reviewer might be a model too

One possible answer is review. Authors produce work quickly; reviewers slow it down, inspect the assumptions, and require the claims to survive contact with another person.

Except the same systems have entered that loop.

An ICML 2024 study examined linguistic changes in reviews from ICLR, NeurIPS, CoRL, and EMNLP. Its [corpus-level estimate](https://proceedings.mlr.press/v235/liang24b.html) was that 6.5 to 16.9 percent of review text had been substantially modified or produced by language models, beyond spell-checking or minor writing updates. That does not identify any particular review as machine-written. Estimated use was higher in reviews submitted close to the deadline and in lower-confidence reviews. The tool was not only helping authors present claims. It was helping reviewers respond to them.

There are constructive versions of this. At ICLR 2025, a [randomized intervention](https://doi.org/10.1038/s42256-026-01188-x) gave reviewers model-generated suggestions about vague language, possible misunderstandings, and unprofessional comments. Some reviewers revised their reports, and blinded evaluators rated the revised reviews as more informative.

This does not demonstrate that a model can replace peer review. It demonstrates that a model can improve a particular part of a human review process when the intervention and outcome are made explicit.

The distinction is important. A second model does not become independent review merely by being a second model. Two systems can share training data, conventions, blind spots, and a preference for the same fluent explanation. Adding agents changes the number of outputs. It does not necessarily change the source of judgment.

An actual review process has many other weaknesses, of course. Human reviewers miss errors, reward familiar methods, disagree, rush, and occasionally do not read closely enough. The point is not that humans provide a magical external check.

The question is what information and incentives each check adds. If every stage evaluates the same generated artifact using the same kind of pattern recognition, the process can become impressively self-consistent without becoming more correct.

## The machine can find something real

There is an easy version of this essay in which every section supplies another example of AI making science worse. It would also be wrong.

FunSearch used a language model to generate candidate programs for mathematical problems. Most candidates were not useful. The system executed them, scored them with an evaluator supplied by the researchers, retained the strongest programs, and used those programs to guide further search. The resulting [Nature paper](https://www.nature.com/articles/s41586-023-06924-6) reported new cap-set constructions and useful bin-packing heuristics.

The language model was valuable because it could search a space of programs in a productive way. It was not asked to decide, in prose, that its own program was mathematically interesting. The programs ran. The evaluator scored them. Other people could inspect the result.

Other systems make weaker claims under different judges. [AI Scientist-v2](https://arxiv.org/abs/2504.08066) generated several machine-learning manuscripts end to end, with humans choosing initial ideas and selecting the best completed run; one manuscript scored above the acceptance threshold at an ICLR workshop. The authors' own inspection also found missing citations, possible data overlap, imprecise method descriptions, incorrect figure interpretations, and code for a calibration technique that was never actually used.

These checks do not provide the same kind of evidence. A program evaluator can reject a candidate against a specified property; passing establishes only that property. An experiment can contradict a prediction. Inspecting a source can show that it does not support a sentence. Human review adds judgment, but a reviewer can still be persuaded by the same polished explanation as everyone else.

What matters is the new information each check contributes. The system becomes more useful when generated candidates encounter evidence that was not produced by the same act of generation and can return an unwelcome answer.

Before treating a check as independent, I now ask three questions: What information does it add that was unavailable to the process that generated the result? What unfavorable answer can it return? Which precise claim would that answer reject?

## Then my own research loop failed twice

I then changed the task. Instead of improving SetScope's song guesses, I asked the system to use the recordings to study improvisation. I had years of labeled audio, access to the usual audio features and increasingly capable embedding models, and an LLM agent that could write scanners, build manifests, generate features, train classifiers, run evaluations, analyze failures, and modify the live application. What had been an implausibly large solo project looked tractable.

The recordings seemed to offer a way to investigate harder questions about improvisation: whether musical features could identify when a performance left its composed structure, whether different forms of jamming had measurable signatures, and whether those signatures generalized across performances. Those questions were more interesting than simple song identification. They were also much harder to define.

I am a PhD-trained systems researcher. I know what a holdout is, and I know why an experimental method has to match its implementation. That is why the first notebook began with an explicit requirement that the same recorded performance could not appear in both training and test. The split report presented that requirement as satisfied. I accepted the report because the point of giving an agent the research process was not to trace every file through every experiment myself. What the report did not disclose was that different encodings, provider copies, and track cuts of the same performances had crossed the boundary.

The metrics looked suspiciously good, so I asked for an audit. Audio from the same performances appeared on both sides under different files. I deleted the notebook and started again.

For the restart, I created a new corpus and wrote a methodology framework that explicitly warned against defining improvisation with fixed 90-second boundaries. I gave that framework to the agent. Its reports did not disclose that the implementation had nevertheless reused a hard-coded 90-second default from an earlier script. Working from those reports, I published two articles and asked friends to spend an evening on a listening study built from the resulting analysis.

Both code defects were small. The consequences were not. I was responsible for publishing the articles and involving other people's time. But the lesson could not be that I should manually reconstruct every split and trace every constant before accepting any result. A system that requires that level of supervision is not running the research process autonomously. It is generating work for a human auditor. The system needed to prevent violations of its rules or report them when they occurred. It had done neither. I deleted the second notebook, pulled the articles, and abandoned the listening study.

We eventually gave the system its original job again: improve the live song guesser. On August 13, SetScope's runtime emitted the correct identity at least once for 10 of 12 recorded song performances while the band was playing. It also missed songs, switched guesses at the wrong time, and exposed failures in the live audio path.

This was a product field test, not a formal whole-show accuracy estimate, and the complete operational record belongs later in this series. Neither audio from that performance nor its completed setlist existed during development. The surviving record shows what reached the controller, not confirmed viewer-visible delivery.

The project would not exist at its current scale without LLMs. The failures taught me that speed changes the location of the work. Producing the next artifact becomes cheap. Establishing what the artifact means, what information entered it, and what could show it is wrong does not.

The machine can participate in research. The harder problem is deciding what it may do without asking, what evidence must survive each iteration, and what can stop a bad result before it becomes the premise of the next experiment. This series is about learning to build those constraints after discovering, repeatedly, that a persuasive report was not one of them.

## What Comes Next

This is the first of seven posts in [The Machine in the Lab](/series/the-machine-in-the-lab/), a field report from the Zabriskie audio-research project.

Part 2 reconstructs the two notebooks I deleted: the questions they tried to answer, the results that looked convincing, and the small implementation choices that invalidated nearly everything built on top of them.

Part 3 is about the boundary we kept crossing. A holdout cannot remain independent if an agent is allowed to inspect it, explain a miss, and then use that explanation to choose the next experiment. The post follows the project from written instructions about data leakage to data roles and permissions the research system could actually enforce.

Part 4 follows the review system we built next. Multiple agents audited the code, checked the methodology, and agreed that the experiment was ready. Every check passed. The experiment still reached the wrong acoustic interpretation for a reason the review process had not been designed to see.

Part 5 moves the experiment into the browser. It covers the audio-capture path, the difference between a classifier result and a viewer-visible decision, and the first genuinely new Goose show the system heard while it was happening.

Part 6 describes what we built instead of another long research prompt: typed experiment state, provenance, explicit permissions, preserved failures, and a promotion boundary that the agent cannot silently cross.

Part 7 returns after Goose's August run with the operational record. It will report the versions that actually ran, the guesses they emitted, the misses and false switches, the capture failures, and the repairs made between shows without compressing an evolving live product into one flattering score.

Next: two notebooks that contained weeks of analysis and almost no result I could still defend.
