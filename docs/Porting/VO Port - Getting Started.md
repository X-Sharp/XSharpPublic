# Porting applications from VO to X#

## General Information

The X# system has become a very mature product regarding porting and maintaining applications 
originally written in Visual Objects (and other xBase dialects are following shortly). We are
 proud to have reached a very high level of compatibility to VO, which is very close to the 
 absolute maximum possible technically and that has greatly helped in successfully porting 
 dozens of applications already!

However, 100% compatibility is not possible, due to important core design differences between 
the Visual Objects system and the .Net Framework. This means that it will always be necessary 
to make manual changes to your code (varying from trivial syntactic changes, to more 
sophisticated code design changes in some cases) after the first port of your application 
(with the VOXPorter tool, which automatically makes many of the necessary changes and 
adjustments), in order to get it to compile and run in X#.

Furthermore, you must be prepared to invest a considerable amount of time to get familiar with
 the new development environment and the .Net Framework. There are two main IDE options for 
 X#, Visual Studio (the market standard) and XIDE (which has a feel very close to that of the 
 VO IDE). No matter which of the two you will choose to use, you need to spend some time 
 getting familiar with it BEFORE attempting to port your VO app. The .Net Framework also also 
 has a learning curve necessary in order to get productive in it. No need to learn everything 
 about it, it is so extremely huge anyway, but you need to be comfortable with it in general, 
 before porting you application.

So, our advice is at first to experiment for a while with the new environment, like you did 
the first time you used Visual Objects as well. Create a couple new simple apps directly in 
VS/XIDE, Experiment with them, adjust them, get familiar with compiling, modifying code and 
binaries (windows, menus etc). When you feel comfortable enough, now it's time to start 
working on porting your application!

## X\# Documentation

Before porting your VO applications, please make sure you have read all the topics in the 
following section of the X# documentation, as it contains valuable information about various 
aspects of porting VO applications to X#:

https://www.xsharp.eu/help/migratingappsfromvotox.html

## X\# Academy tutorials

Please also view those two tutorials, demonstrating, hands on, the porting process and 
showing how to solve various obstacles in the process:

https://www.youtube.com/watch?v=yduay2fc-nE
https://www.youtube.com/watch?v=A-zhcsYuas8

## X\# Forum

Please do not hesitate to post any questions, clarifications or ask for help on any matter in 
our forum! Many experienced X# developers are always happy to give peer to peer support, and 
you can also get direct support from the X# development team members, if you become a 
subscriber to the Friends Of XSharp [(FOX) program](https://www.xsharp.eu/store/shop/FOX-Subscription-c82913713)

https://www.xsharp.eu/forum

## 3rd party libraries

In X#, we have already ported for you the standard VOSDK libraries (GUI classes, RDD classes, 
Win32 API Library etc), so you can use them out of the box. Most VO applications though also 
use a number of 3rd party libraries, like bBrowser, ReportPro, FabTools and others. For 
those, you need to use their .Net equivalents. Fortunately, all the most commonly used 3rd 
party libraries have already been ported to X#, you can find downloading/purchasing 
instructions at the end of this document. In case you are using a 3rd party library that is 
not included in the list, please let us know and we will make sure to make this available as 
well!

## We can port your application for you

If you prefer not spending the time to port your VO application(s) on your own, we can do it 
for you; we have successfully ported several applications already, so you can rest assured 
that you will get a fully working solution at minimum effort from your part. At a determined 
fee (depending on the size and complexity of your VO system), you can send us your source 
code and we will send you back a ported version fully working in X#!

If you are interested in this service, please contact us at info@xsharp.eu

## After porting your app

You have ported your VO application to X#, well done! This has already given a lot more years 
of life to it, as it's now developed in a modern, very widely used and supported environment. 
It should also be more stable and robust than it used to be, due to the .Net Framework's far 
superior Memory Management and Garbage Collector, compared to that of VO.

From now on, you can continue its development purely in X#, and make use of the unlimited 
amount of features and tools that .Net has to offer. Using 64 bit numeric variables, the 
Decimal data type for currency, collections, many new language constructs like interfaces, 
events, enumerations and so many others, easy multithreading, unicode strings out of the box, 
huge amount of classes available in the framework itself and for 3rd party vendors (all 
libraries written in any .Net language are automatically available to X#, too!). The list of 
new features endless, and waiting for you to explore and incorporate in your application 
development from now on!

# 3rd Party products

## ReportPro

RP2 and RP3 (including classmate) can be bought from 
https://www.xsharp.eu/store/shop/X-3rd-party-products-c110612018. If you have a receipt or 
other proof of purchase of any VO version of RP2/RP3, please contact us at info@xsharp.eu 
for a discount!

## bBrowser

X# version of bBrowser needs to be purchased from https://bbrowser.net

## FabTools

Full source code of FabTools can be downloaded from GitHub - X-Sharp/FabTools: Port to X# 
of most of FabTools Libraries .
Binary only version can be downloaded from the X# downloads area: 
https://www.xsharp.eu/itm-downloads/download?path=general%252FTools%252FFabTools4XSharp.zip

## SEUIXP

X# version needs to be purchased from SEUIXP - X# version Sourcecode + Binaries (xsharp.eu)

## Xs2Ado (VO2Ado for X\#)

X# version needs to be purchased from Xs2Ado Sourcecode + Binaries (xsharp.eu)

## RightSLE

Wolfgang Riedmann had kindly ported and uploaded Willie Moore's RightSLE control to X# 
here:
Source code: https://www.riedmann.it/download/RightSLE-VOGUI.zip
Binaries: https://www.riedmann.it/download/RightSLE-VOGUI_bin.zip

# Other Resources

## The X# Documentation Project

Wolfgnag Riedmann has gathered a lot of varied resources regarding X# in his website:
https://docs.xsharp.it/doku.php