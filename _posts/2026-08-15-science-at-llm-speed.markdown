---
layout: post
title: "Science at LLM Speed"
subtitle: "Research-like output is cheap. Valid claims still need an outside judge."
date: 2026-08-15 14:00:00 -0400
group: ai
series: lab
permalink: /series/the-machine-in-the-lab/science-at-llm-speed/
categories: ai research zabriskie agents
---

Earlier this year, the program chairs for one of the largest machine-learning conferences found references in submitted papers to publications that did not exist.

This was not a thought experiment about what a language model might do. It was not a benchmark in which researchers asked a model to write a literature review and then counted the invented citations. These were papers submitted to ICLR 2026, accompanied by bibliographies that were supposed to describe prior work. The conference built a screening system, sent the flagged references to area chairs, and then had the program chairs check them again. Every paper with a confirmed hallucinated reference was desk rejected.

The most interesting part of the [program chairs' account](https://blog.iclr.cc/2026/03/31/a-retrospective-on-the-iclr-2026-review-process/) is how much work it took to establish that a paper in a bibliography was not a paper. The automated system produced false positives. Translated titles looked suspicious. At least three people reviewed every confirmed case. The conference did not trust one detector to decide whether a citation existed.

The authors of the papers apparently had.

This is a strange failure because background research is one of the activities these systems appear to make almost effortless. Ask for the important papers in an area and a model will produce authors, titles, dates, summaries, and a tidy account of how the work fits together. The answer has the shape of a literature review before anyone has done the work of reviewing the literature.

Sometimes the papers are real. Sometimes a real paper is attached to a claim it does not support. Sometimes the title, authors, or venue are slightly wrong. And sometimes the entire reference was generated because a plausible citation completed the paragraph.

All four cases look approximately the same on the page.

## The literature is no longer entirely about things that happened

We knew this behavior existed before it began appearing in conference submissions. In 2023, William H. Walters and Esther Isabelle Wilder asked GPT-3.5 and GPT-4 to produce short literature reviews across 42 topics. They checked 636 generated citations. In their [controlled study](https://www.nature.com/articles/s41598-023-41032-5), 55 percent of the GPT-3.5 citations and 18 percent of the GPT-4 citations referred to works the researchers could not verify as existing. Many citations to real work also contained substantial bibliographic errors.

Those numbers do not tell us what fraction of references in current scholarship were written by a model. They describe particular models, prompts, and topics in a controlled experiment. A broken citation in a paper does not contain a record of the tool that produced it. People invented, mangled, and copied references long before ChatGPT.

But the ICLR cases establish the part that matters here: nonexistent references made it through whatever process their authors used to produce a submission for a major research conference. The scholarly form was present. The scholarship that the form purported to reference was not.

The obvious response is to connect the model to a search system. OpenScholar, for example, retrieves passages from a corpus of 45 million open-access papers, generates an answer with citations, and then uses a separate checking process to refine the result. In the authors' [evaluation](https://www.nature.com/articles/s41586-025-10072-4), the system substantially improved the citation accuracy and correctness of a general-purpose model on scientific synthesis tasks.

This is better, but the reason it is better matters. The model did not become more trustworthy by writing a more confident explanation of its sources. The system changed the task. Candidate claims had to be attached to retrieved passages. Citations could be checked against documents that existed outside the generated answer. Expert readers could inspect whether the source supported the sentence.

Retrieval does not make a literature review true. A real paper can still be misunderstood, cited out of context, or asked to carry a larger claim than its experiment supports. What retrieval supplies is the possibility of contradiction. There is now something outside the paragraph that can show the paragraph is wrong.

That difference between fluent support and evidence capable of contradiction is the problem I kept encountering in my own work.

## More research-like work, produced faster

The citation problem is unusually easy to see because existence is a fairly crisp property. Either the referenced document can be found or it cannot. Most research decisions are not like that.

A model can now help formulate a question, search for related work, write experiment code, select a statistical test, produce a figure, explain the figure, draft the limitations, and review the finished manuscript. Each output can become the input to the next step. A single person can move through activities that once required more time, more specialized assistance, or both.

While I was writing this, Jeff Dean, Sanjay Ghemawat, Oriol Vinyals, and Quoc Le left Google to form a company called [Discovery Loop](https://www.axios.com/2026/08/06/googles-ai-leadership-shuffle). Public descriptions say its goal is to [automate experimental loops](https://www.itpro.com/business/leadership/deepmind-ceo-demis-hassabis-steps-aside-amid-google-leadership-shake-up) in science and engineering: propose an experiment, run it, evaluate what happened, and use the result to decide what to try next. The details of the system are not public, and I do not want to reverse-engineer an architecture from a company announcement. The name is useful because it identifies the thing that has changed.

The model is no longer only helping with a research task. It is becoming capable of moving the project around the loop.

My audio project is obviously not operating at the scale implied by that company. Goose song identification is probably not their launch domain. But the underlying systems problem is recognizable. Once an agent can propose an experiment, implement it, run it, read the result, and modify the next experiment, research velocity becomes a property of the loop rather than any one model response. So do the failures. A contaminated result can alter the next hypothesis. A convenient proxy can become the objective. A component test can be promoted into a claim about a system that never ran.

There is emerging evidence that this has changed scientific production, although the size of the change is difficult to measure cleanly. A [large study of preprints, peer reviews, and document-access records](https://doi.org/10.1126/science.adw3000) associated inferred LLM use with increased manuscript output. A subsequent [methodological comment](https://arxiv.org/abs/2605.17979) identified an important selection problem: authors with high-output months give a classifier more opportunities to decide that adoption occurred. The direction is plausible. The largest causal interpretation is not yet settled.

I find the narrower observation more useful anyway. LLMs have made it cheaper to produce an artifact that looks like the output of a research process. That artifact might be a paper. It might also be a methodology page, an interactive analysis, a benchmark, a data product, or a long post with equations and charts.

Some of the most interesting versions appear outside journals and conferences. A person with a question and a dataset can now get meaningful help writing data-cleaning code, selecting features, fitting models, and interpreting results. In a participatory study of generative-AI-assisted data analysis, [Drosos and colleagues](https://doi.org/10.1145/3663384.3663389) found that people used a model for information gathering, hypothesis generation, and analysis strategy. They also struggled to supply the right context and verify what the system returned. Checking an answer could require as much work as obtaining the answer without the model.

Useful analysis does not have to occur inside a university or become a paper. A fan project may rank performances, organize an archive, publish its formulas, and offer an excellent discovery tool. Reproducing the score can be sufficient evidence that the product implements its stated rule.

A broader empirical interpretation asks for something else. If a project says its score reveals a property of improvisation, the labels, measurement unit, and relationship between the score and that musical property become part of the claim. The publication venue does not settle whether the evidence is adequate. Neither does the presence of data, equations, transparent code, or polished charts.

This category existed before LLMs. What changes now is the cost of producing the complete package. Code, prose, caveats, visualizations, and a memorable result can arrive together, quickly enough that their coherence feels like evidence that the underlying empirical work occurred.

My own project produced exactly that kind of convincing package, more than once. The form did not create the errors. It made them harder to notice.

## The reviewer might be a model too

One possible answer is review. Authors produce work quickly; reviewers slow it down, inspect the assumptions, and require the claims to survive contact with another person.

Except the same systems have entered that loop.

An ICML 2024 study examined linguistic changes in reviews from ICLR, NeurIPS, CoRL, and EMNLP. Its [corpus-level estimate](https://proceedings.mlr.press/v235/liang24b.html) was that 6.5 to 16.9 percent of review text had been substantially modified or produced by language models, beyond spell-checking or minor writing updates. That does not identify any particular review as machine-written. Estimated use was higher in reviews submitted close to the deadline and in lower-confidence reviews. The tool was not only helping authors present claims. It was helping reviewers respond to them.

There are constructive versions of this. In a [study covering papers submitted to Nature-family journals and ICLR](https://doi.org/10.1056/AIoa2400196), researchers compared GPT-4-generated feedback with human reviews. A majority of participants in a prospective study found the generated feedback helpful, although the model commented on novelty much less often than human reviewers. At ICLR 2025, a [randomized intervention](https://doi.org/10.1038/s42256-026-01188-x) gave reviewers model-generated suggestions about vague language, possible misunderstandings, and unprofessional comments. Some reviewers revised their reports, and blinded evaluators rated the revised reviews as more informative.

These are not demonstrations that a model can replace peer review. They are demonstrations that a model can improve parts of a human review process when the intervention and outcome are made explicit.

The distinction is important. A model producing a paper and another model saying the paper looks plausible is not independent review. Two systems can share training data, conventions, blind spots, and a preference for the same fluent explanation. Adding agents changes the number of outputs. It does not necessarily change the source of judgment.

An actual review process has many other weaknesses, of course. Human reviewers miss errors, reward familiar methods, disagree, rush, and occasionally do not read closely enough. The point is not that humans provide a magical external check. The point is that we have to ask what information and incentives each check adds. If every stage evaluates the same generated artifact using the same kind of pattern recognition, the process can become impressively self-consistent without becoming more correct.

## The machine can find something real

There is an easy version of this essay in which every section supplies another example of AI making science worse. It would also be wrong.

FunSearch used a language model to generate candidate programs for mathematical problems. Most candidates were not useful. The system executed them, scored them with an evaluator supplied by the researchers, retained the strongest programs, and used those programs to guide further search. The resulting [Nature paper](https://www.nature.com/articles/s41586-023-06924-6) reported new cap-set constructions and useful bin-packing heuristics.

The language model was valuable because it could search a space of programs in a productive way. It was not asked to decide, in prose, that its own program was mathematically interesting. The programs ran. The evaluator scored them. Other people could inspect the result.

Other systems make different claims. OpenScholar uses retrieval and citation checking to improve scientific synthesis. [AI Scientist-v2](https://arxiv.org/abs/2504.08066) generated several machine-learning manuscripts end to end, with humans choosing initial ideas and selecting the best completed run; one manuscript scored above the acceptance threshold at an ICLR workshop. The authors also found citation inaccuracies, possible data overlap, imprecise method descriptions, and code for a calibration technique that was never actually used. Google's [Co-Scientist system](https://www.nature.com/articles/s41586-026-10644-y) has generated and ranked biomedical hypotheses, some of which were taken into experimental validation.

These results should not be flattened into either "AI can do science" or "AI cannot do science." They represent different systems, different degrees of autonomy, and very different judges. Workshop review is not a wet-lab experiment. Expert preference is not mathematical execution. A retrieved citation is not evidence that the cited claim is correct.

The strongest examples do share a structure. Generation is coupled to something that can refuse the generated answer: a program evaluator, a source document, a blinded human judgment, a formal proof obligation, or an experiment in the world.

The system is useful not merely because it can produce candidates at speed, but because those candidates eventually encounter a process that does not care how persuasive the generated explanation sounds.

## A small audio lab enters this moment

I started the Zabriskie audio-research project with a practical question. I wanted a computer to listen to a live Goose show and identify the song while the show was happening, without waiting for an external setlist.

I had years of labeled recordings. I had access to the usual audio features and increasingly capable embedding models. I also had an LLM agent that could write scanners, build manifests, generate features, train classifiers, run evaluations, analyze failures, and modify the live application. What had been an implausibly large solo project looked tractable.

Then the corpus pulled the project sideways. The recordings seemed to offer a way to investigate harder questions about improvisation: whether musical features could identify when a performance left its composed structure, whether different forms of jamming had measurable signatures, and whether those signatures generalized across performances. Those questions were more interesting than simple song identification. They were also much harder to define.

I am a PhD-trained systems researcher. I know what a holdout is. I know why an experimental method has to match its implementation. I know that a result needs a claim it can actually support. I wrote methodology documents explaining several of those requirements before the agent began working.

It still went wrong.

The agent produced code that ran, clean-looking metrics, detailed figures, interpretations, two long articles, and a listening study. The work passed through the kinds of checks I had asked it to perform. From inside the project, the volume and coherence of the output looked like progress.

Underneath the first notebook, performances from the same source had crossed the training and test boundary. Underneath the second, a 90-second default that the written method had explicitly warned against silently defined which parts of a track counted as improvisation. Both mistakes were small in code. Both propagated through almost everything that followed.

I eventually deleted the two notebooks on the same evening.

That experience did not teach me that LLMs cannot contribute to research. The project would not exist at its current scale without them. It taught me that speed changes the location of the work. Producing the next artifact becomes cheap. Establishing what the artifact means, what information entered it, and what could prove it wrong does not.

The question is no longer whether the machine can participate in research. It plainly can. The question is how to build a discovery loop that can distinguish progress from a persuasive trip around its own assumptions: which decisions it may make silently, which checks add independent information, what state must survive the next iteration, and who is allowed to say that a result is ready to become a claim.

## What Comes Next

This is the first of seven posts in [The Machine in the Lab](/series/the-machine-in-the-lab/), a field report from the Zabriskie audio-research project.

Part 2 reconstructs the two notebooks I deleted: the questions they tried to answer, the results that looked convincing, and the small implementation choices that invalidated nearly everything built on top of them.

Part 3 is about the boundary we kept crossing. A holdout cannot remain independent if an agent is allowed to inspect it, explain a miss, and then use that explanation to choose the next experiment. The post follows the project from written instructions about data leakage to data roles and permissions the research system could actually enforce.

Part 4 follows the review system we built next. Multiple agents audited the code, checked the methodology, and agreed that the experiment was ready. Every check passed. The live application still failed for a reason the review process had not been designed to see.

Part 5 moves the experiment into the browser. It covers the audio-capture path, the difference between a classifier result and a viewer-visible decision, and the first genuinely new Goose show the system heard while it was happening.

Part 6 describes what we built instead of another long research prompt: typed experiment state, provenance, explicit permissions, preserved failures, and a promotion boundary that the agent cannot silently cross.

Part 7 returns after Goose's August run with the operational record. It will report the versions that actually ran, the guesses they emitted, the misses and false switches, the capture failures, and the repairs made between shows without compressing an evolving live product into one flattering score.

Next: two notebooks that contained weeks of analysis and almost no result I could still defend.
