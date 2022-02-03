# Biblio
TLDR: an experimental dsl for bibliography creation


## Introduction

Biblio is a DSL for researchers who desire a way to easily organize, cite, and maintain a bibliography of sources. Current solutions to one part of the problem—formatting—are the Bibtext extension to Latex and various online tools such as easyBib. With these tools the user inputs the fields of their source and receive the information formatted to their bibliographic style. This removes the mental effort of referring to the guidelines of MLA or APA, but offers nothing in organizing sources. Another solution is the software Zotero, which organizes sources as a hierarchal filesystem and exports various citations and bibliography styles. However, because our minds often make random associations to various bits of information we encounter, it is limiting to think hierarchically. In order to model our research organization in line with how our minds intuitively makes connections, Biblio implements a bibliographic map in which the user can link sources.

Bibframe, which is only in the design stage and has yet to be implemented as a tool, is a robust data model for describing bibliographic sources that takes advantage of this mapping organization method. Because it’s intended audience is librarians, however, it provides many details that are superfluous and even distracting to a researcher. For example, as necessary for cataloging, Bibframe creates abstractions for each Instance and Item related to a Work. Where the Work is the essence of the information, the Instance could be a specific publishing of it and the Item a physical copy that exists in a library [1]. While the publishing year and copy of an item may be important to the researcher, they are likely not interested in keeping a running list of all publishings and copies like the librarian. Still, the needs of a researcher and librarians clearly overlap, and so I take some inspiration from Bibframe’s model. 

While a programming language may seem like an unlikely tool for many researchers and even daunting for those without experience in programming, I hope the simplicity of the development environment and concrete syntax makes it easy to use and an attractive option to all. With the expansion of knowledge coming from the increasing connectivity of people through the web, who’s first users were academic researchers, research will require more powerful tools to organize and comprehend information.

## Software Ecosystem

Biblio is an interpreted, stand-alone DSL so that it has a low barrier for users—who may not have experience programming—to get started. Because one of Biblio’s main features is organizing sources, it is designed to be a tool used throughout the research process, which is best achieved by an interactive prompt as the development ecosystem. While not implemented currently, the state of the Biblio interpreter is saved so that a researcher can load it and continue their work later.

## Working Example

When beginning a new project, the style is automatically set to MLA. However, I am going to change the style to Chicago.

`> style Chicago`

Because I have done some previous readings, I know I want to include a specific source, so I add all required the information of it.

`> source defronzo {author: ‘Defronzo, James’, title: ‘Revolutions and Revolutionary Movements’, publisher: ‘Westview Press’, year: ’2014’`

I found a source referenced in Defronzo so I am going to add the author and title to my list and link it to Defronzo so I remember to revisit it later.

`> source skocpol {author: ‘Skocpol, Theda’, title: ‘States and Social Revolutions’}`

`> link defronzo skocpol ‘classical but flawed theory on revolutions’`

After reading skocpol, I decide I want to use it for my research, so I fill out the rest of the fields. However, I don’t remember how I named the source, so I’m going to look through the sources linked to defronzo.

`> graph defronzo
Defronzo, James. Revolutions and Revolutionary Movements. Westview Press, 2014.
Links skocpol ‘classical but flawed theory on revolutions’
Links fitzpatrick  ‘russian revolution emphasis on security forces’`

I found skocpol, so I will end my search.

`> end`

However, if I wanted to follow one link to see what it is like to I could type. Remembering to type end when to stop following.

`> follow fitzpatrick
Fitzpatrick, Shiela. The Russian Revolution. Oxford University Press, 2008.
Links suny ‘sociological theory on revolutions’`
`> end`

However, Now I want to update skocpol.

`> update skocpol {publisher: ‘Cambridge University Press’, year: ‘2015’}`

I continue to enter sources throughout my research project until I am at the end and want my bibliography.

`> bibliography`

## Abstractions and implementation

Implemented in Haskell, Biblio establishes an IO loop with an initial program state, which is represented by an Abstract Data Type of the style and map of sources. In the beginning, the map of sources is empty and the style is set to MLA, so there is always safe printing. The parser interprets each line and passes the state to the main program, which then returns the new program state. The following are the rules for a Biblio expression.
 
    exp = <field> <collection> String
        | set <collection> <condition>
        | link <collection> String
        | graph <name> <following> end
        | style <styles>
        | bibliography <collection>
        | bibliography <name>
        | source <name> map-of <fieldPair>
        | update <name> map-of <fieldPair>
    
    following = NOTHING
              | follow <name>
    
    name = USERDEFINED


The working example above demonstrates setting the style, creating and updating a source, following links in the graph of sources, and printing a bibliography. The `set` expression lets a user refer to a collection of sources that meet a condition and a `field` expression is a method that provides the meta-information for that field for a given collection.

## Evaluation

Biblio’s current software ecosystem and concrete syntax is a simple-easy to use tool for research with little to no programming experience. It is, however, important to find to strike the right balance between simplicity and power in a language. The current features offered by Biblio are unique to any other research tool currently in use, however, further language design work is necessary in order to make such a tool attractive to users.

## Future Work

As mentioned previously, Bibframe is a data model for bibliographic descriptions that has yet to be implemented. While Biblio was not intended to build out Bibframe’s model, it does form a small basis that could be expanded in the future. Because the librarian would have to handle more data than the researcher, however, this would only be efficient as a compiled language.

Further work for Biblio as it currently stands could be finding ways to integrate it with current tools used by researchers. While it is important this doesn’t come at the cost of Biblio’s simplicity and strong abstractions, one concrete idea could be generating Latex code.

Another potential could be integrating Biblio into word processors so as to automate the creation and maintenance of citations. So when a source is updated, so are all of its occurrences in the paper. This feature would be most valued in long-form research papers or books.

Because Biblio has been designed for people with little to no programming experience, it does not give the user the ability to extend the language themselves. However, future work may include defining their own bibliographic style.

## Sources

[1] Overview of the Bibframe 2.0 Model. Library of Congress. April, 2016. https://www.loc.gov/bibframe/docs/bibframe2-model.html 