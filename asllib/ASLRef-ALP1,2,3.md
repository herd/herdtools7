# ASLRef-ALP1 Jira tickets

## ASL-628 - Type-system to fix
I don't uderstand this ticket.

## ASL-627 - Symbolic ranges
ASLRef code: fixed.
Transliteration: pending.

## ASL-623 - Type of i in for i = a to b
We are waiting to see what is required in Proto2.

## ASL-622 - Loop/recursion limits annotation
ASLRef code: requires implementation, but we still haven't encountered loop limits in Harry's code.
Transliteration: required.

## ASL-621 - Grammar
The ticket lacks description, but at the minimum, we need to fill in the syntax reference
with the grammar implemented by ASLRef.

## ASL-620 - Rename underconstrained
Waiting for discussion to resolve.

## ASL-619 - Rename real to rational
Probably not the most urgent ticket.
Perhaps we should temporarily map rational to the same token as real
and when Harry's code switches to rational, eliminate real.

## ASL-615 - ASLRef to type-check prototype 2
Waiting for answers from Harry.

## ASL-611 - Readability review
Let's wait and see how Proto2 develops.

## ASL-610 - Support for prototype 2
Let's wait and see how Proto2 develops.

## ASL-597 - Tuples in common ancestor requires type-satisfaction on each element
The work on ASLRef's side is done.
Waiting for Jade to indicate how to mark the ticket as being closed on our side
and pending work on ArchEx's side.
I updated the status to be "In Progress".

## ASL-595 - How is the under-constrained domain meant to be handled?
This is handled by the ASLRef code but needs transliterating the subsumption testing.
I answered Skye and asked him whether this makes sense and we confirmed that it does
but may be complicated to implement in ArchEx.
I updated the status to be "In Progress".

## ASL-591 - Rename "real" token to "rational"
Duplicate. Closed.

## ASL-588 - Make the rule YHRP more specific
Hadrien: I think this is a LRM problem, or maybe a problem when we want to explain what the standard library does.
Roman:  I think this is part of the bigger problem of classifying different types of errors.
        I don't know whether ALP1 is the right label though.

## ASL-584 - Named types should be eligible for bitwidth types
Hadrien: This is partially accepted by ASLRef. This is the following expression definitions for immutable variables PR
        (https://github.com/herd/herdtools7/pull/894).
Roman: ****

## ASL-582 - Rule QDQD incorrectly states that multiple identifiers can be initialized with a single expression
Hadrien: I think this is a LRM problem. There is a current item in my todo list to check that multiple global declarations are correctly handled by ASLRef.
Roman: What Hadrien said. I will follow up in ASL Typing Reference.

## ASL-578 - '!' operator precendence should be less than 'IN'
Hadrien: This is a proposal. However I think this is already accepted by ASLRef.
Roman: I added a comment confirming that this is the case in ASLRef. I will document this in ASL Syntax Reference.

## ASL-574 - Confusing way that an identifier, declared as a constant, has different 'values'.
Hadrien:
Roman: ****

## ASL-571 - Should Rtphr be reworded/removed?
Hadrien: This is LRM problem
Roman: This is Closed.

## ASL-569 - Remove restriction on tabs
Hadrien: ASLRef grammar does not consider tabs illegal and treat them as any other space. If we want to change that, it is possible, but why?
Roman: This is Closed.

## ASL-565 - Implement first proposal for type-checking
Roman: In progress.

## ASL-539 - (partner feedback) improve nested bitfields example
Hadrien: This is about the LRM, although maybe
        @roman.manevich  we need to check that in the transliteration the explanation text for nested bitfields cover those.
Roman: I will work on this for ALP2.

## ASL-532 - Rule PTNG is incomplete
Hadrien: This is about the LRM.
Roman: this is Closed.

## ASL-526 - info TRPS is confusing about Unreachable()
Hadrien:  think this is a LRM problem, or maybe a problem when we want to explain what the standard library does.
Roman: we need to implement a static analysis to check that for every function and each of its simple paths,
it either returns a value or calls Unreachable().

## ASL-520 - Type safety issues with under-constrained integers
Roman: This is Closed.

## ASL-519 - Clarification of access pattern application order
Hadrien: This is a mix between side effects (so not ALP1) and aspect proposal (so not in ASL1).
Roman: this is Closed.

## ASL-504 - Replace informal definition of static equivalence with formal one
Roman: In progress.

## ASL-501 - Standardize the syntax for storage declarations (local and global)
Hadrien: This is a proposal. I don't know what to do with this.
Roman: The proposal makes sense, but this is probably not a high priority one. Let's postpone this.

## ASL-492 - Fix section 3.9 to refer to domains of integer types rather than structure of integer types
Roman: This is Closed.

## ASL-478 - Clarify usage (or illegality) of CTCs outside subprograms
Roman: I need to finish transliterating the chapter on specifications to make this precise.

## ASL-476 - What is the domain of structured types?
Roman: This is Closed.

## ASL-416 - Clarify that bitfield widths can't be negative
Hadrien: This is either LRM or specs problems.
        @roman.manevich you probably have more insightful comments on this than me.
Roman: Added a comment that bitvector widths are indeed assumed to be non-negative and that we will follow up
        on this both in the references and in the code.

## ASL-375 - LRM examples should be tested
Hadrien: I think that we could add the examples from the LRM into our testing infrastructure, but :
        Is this something we want to do for ALP1?
        Who should do that?
Roman: I will take care of this.

## ASL-364 - Initialization of variables of record type with empty record constructors
Hadrien: I don't think this is allowed.
Roman: this is more of a feature request really. I answered Kostas with a comment.
       I moved it to ASLRef-Next.

## ASL-321 - Should a Getter return type match with a Setter RHS?
Hadrien: I think this is a LRM problem.
Roman: I'll ask Harry whether we can close this.

## ASL-303 - ASL specifications versus programs
Hadrien: This is a proposal, that we can or cannot do. I don't have opinions on this.
Roman: Sounds like a reasonable proposal. I commented on the ticket.

## ASL-296 - Rule ITFSZ does not take integers into account
Hadrien: This is a LRM problem.
Closed.

## ASL-261 - "Structured Types" not defined
Hadrien: This is a transliteration problem I think?
         @roman.manevich I will let you comment on this one.
Roman: I've been meaning to define it in the references. I think it is actually useful and I will add it
        to ASL Typing Reference.

## ASL-256 - Badly worded bitslice bounds check rule KTBG
Hadrien: This is a LRM problem.
Roman: agree.

## ASL-201 - Examples in Subtypes section (4.2.1) broken
Roman: This is Closed.

## ASL-199 - Specified limits of parsing and execution
Hadrien: I do not not know what to do with this but it does not seem to be an issue with our tools?
Roman:  This seems to be an effort to put bounds on elements of the language and the environment
        for tools operating on specifications. I think that as a reference implementation and documentation,
        we shouldn't place bounds. Anyway, this is probably for nor either Alpha or Beta.
No one is assigned. We should find someone to either assign or change the label to ASLRef-Next.

## ASL-165 - Array declarations, is there still special syntactic sugar? Bug in current LRM
Hadrien: 2 things:
        The syntax to declare arrays like this does not exist anymore
        There is today no syntax to initialise arrays, but probably this should be another Jira, and probably not for ALP1?
Roman: agree.

## ASL-87 - Formalise the semantics of ASL
Roman: This is mostly done, but the next iteration will enable closing this ticket.
