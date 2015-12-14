# golfml

XML Specifications for Golf Courses and Score Data Exchange

## Attribution

This project has been exported from [Google Code]
(https://code.google.com/p/golfml/).

The original project has been created by
[Pierre Mareschal](mailto:pierre.mareschal@gmail.com)
and [other contributors](https://code.google.com/p/golfml/people/list).

We have exported this project to preserve it in GitHub,
pending the [announced closing of Google Code on January 15th, 2016]
(http://google-opensource.blogspot.fr/2015/03/farewell-to-google-code.html).

## Code License

[Mozilla Public License 1.1](http://www.mozilla.org/MPL/)

## Labels

golf, xml, scorecard, golfcard, statistics, score, golfcourse, golfhole

## Links

* [Start Here]
  (https://github.com/eric-brechemier/golfml/wiki/Welcome)
* [GolfML Discussion Group]
  (https://groups.google.com/forum/#!forum/golf-markup-language)

## Original Description

The purpose of GolfML is to provide a [XML](http://en.wikipedia.org/wiki/XML)-based file format for the exchange of golf-related data.

There are numerous golf scorecard programs and online web sites that allow golfers to browse course information, print scorecards, keep track of scores, and analyze statistics, or view a Google Earth fly-over of each hole.

Most programs or web sites use proprietary data representation and only few of them allow for data exchange of course descriptions such as scorecard, hole par, length and handicap strokes. If available, data import or export is often performed in site- or application-specific proprietary format.

We promote the use of a vendor-neutral, open data format for the exchange of golf-related data like golf course descriptions, or golfer scorecards.

The markup language will focus on:

  1. The description of golf courses, including geo-spatial data
  1. The golfer's logbook, with scorecards and statistics

The first part of this project defines a uniform data format to present golf course information such as name, address, number of courses, available facilities, course description, individual hole tee length, par, handicap strokes, etc. This data format also allows definition of geographic areas such as GPS coordinates (of tees, holes, trees and traps...) or surfaces (such as fairways, bunkers, greens, lake...). It also permit references to files like pictures, video, or other media.

The second part of the project defines a data format to allow golfer to record scores, statistics, and golf equipment. Scores can be as simple as stroke counts or as complex as location-based individual stroke record.

This project will not deliver software code. It will deliver a vocabulary, XML specifications and references to allow for data exchange between golf software applications.

[Start Here](http://code.google.com/p/golfml/wiki/Welcome) or look at [Typical Uses of GolfML](wiki/Workflow)
