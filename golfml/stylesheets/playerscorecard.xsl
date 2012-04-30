<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0"
                                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"        
                                xmlns:g="http://code.google.com/p/golfml"
        >
<!-- custom-scorecard.xsl

DESCRIPTION

        Demonstration stylesheet for GolfML.
        From a player's unique identifier (name and birthdate) and from a tee-set's unique identifier, generates a custom scorecard in HTML


VERSION
        $Revision$


HISTORY
        Sep 2009: Created.

-->
        <xsl:output method="html" indent="yes"/>

        <xsl:param name="golfer.name">Bob</xsl:param>
        <xsl:param name="golfer.birthdate">2000-01-01</xsl:param>
        <xsl:param name="country-club.name">Abama Golf and Spa resort</xsl:param>
        <xsl:param name="country-club.golf-course.name">Main Course</xsl:param>
        <xsl:param name="tee-set.name">Regular</xsl:param>
        <xsl:param name="personal-par">yes</xsl:param>


        <xsl:template match="/">
                <html>
                        <head>
                                <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
                                <link href="golfml.css" rel="stylesheet" type="text/css"/>
                                <title>GolfML Custom Scorecard</title>
                        </head>
                        <body><!-- /g:golfml/g:country-club[g:name=$country-club.name]/g:golf-course[g:name=$country-club.golf-course.name] -->
                                <xsl:apply-templates select="/g:golfml/g:country-club[g:name=$country-club.name]/g:golf-course[g:name=$country-club.golf-course.name]">
                                        <xsl:with-param name="handicap" select="number(/g:golfml/g:player[g:name=$golfer.name]/g:current-handicap)"/>
                                </xsl:apply-templates>
                        </body>
                </html>
        </xsl:template>


        <xsl:template match="g:golf-course">
                <xsl:param name="handicap"/>

                <h1>Custom scorecard for: <xsl:value-of select="$golfer.name"/> (current handicap: <xsl:value-of select="$handicap"/>)</h1>
                <table  class="course-data">
                        <caption><xsl:value-of select="../g:name"/> (<xsl:value-of select="g:name"/>)</caption>
                        <thead>
                                <tr>
                                        <td>Tee Color</td>
                                        <td>Rating</td>
                                        <td>Slope</td>
                                        <xsl:if test="count(g:holes)>0">
                                                <xsl:for-each select="g:holes/g:hole">
                                                        <xsl:sort select="@number" data-type="number"/>
                                                        <td>
                                                                <xsl:element name="a">
                                                                        <xsl:attribute name="href"><xsl:value-of select="g:media/g:url"/></xsl:attribute>
                                                                        <xsl:attribute name="alt"><xsl:value-of select="g:media/g:name"/> (<xsl:value-of select="g:media/@type"/>)</xsl:attribute>
                                                                        <xsl:value-of select="@number"/>
                                                                </xsl:element>
                                                        </td>
                                                </xsl:for-each>
                                        </xsl:if>
                                        <xsl:if test="count(g:holes)=0">
                                                <xsl:for-each select="g:tee-set[position() = 1]/g:tee">
                                                        <xsl:sort select="@number" data-type="number"/>
                                                        <td>                                            
                                                                <xsl:element name="a">
                                                                        <xsl:attribute name="href">
                                                                                <xsl:value-of select="g:holes/g:hole[@number=1]/g:media/g:url"/>
                                                                        </xsl:attribute>
                                                                        <xsl:attribute name="alt">
                                                                                <xsl:value-of select="g:holes/g:hole[@number=1]/g:media/g:name"/>
                                                                                (<xsl:value-of select="g:holes/g:hole[@number=1]/g:media/g:name/@type"/>)
                                                                        </xsl:attribute>
                                                                        <xsl:value-of select="position()"/>
                                                                </xsl:element>
                                                        </td>
                                                </xsl:for-each>
                                        </xsl:if>
                                        <td>Out</td>
                                        <td>In</td>
                                        <td>Total</td>
                                </tr>
                        </thead>
                        <tbody> 
                                <xsl:apply-templates select="g:tee-set[@name=$tee-set.name]">
                                        <xsl:with-param name="handicap"><xsl:value-of select="number($handicap)"/></xsl:with-param>
                                </xsl:apply-templates>
                        </tbody>
                </table>
        </xsl:template>


        <xsl:template match="g:tee-set">
                <xsl:param name="handicap"/>
                
                <xsl:param name="slope"><xsl:value-of select="number(g:qualification/g:qualification-usga/g:slope)"/></xsl:param>
                <xsl:param name="strokes"><xsl:value-of select="ceiling($handicap * $slope div 113)"/></xsl:param>
                <xsl:param name="holes-all" ><xsl:value-of select="floor($strokes div 18)"/></xsl:param>
                <xsl:param name="holes-some"><xsl:value-of select="$strokes mod 18"/></xsl:param>
                
                <tr class="handicap">
                        <td colspan="3" class="label">Handicap</td> 
                        <xsl:for-each select="g:tee">
                                <xsl:sort select="@number" data-type="number"/>
                                <td>
                                        <xsl:value-of select="g:handicap-stroke"/>
                                </td>
                        </xsl:for-each>
                </tr>
                <xsl:element name="tr">
                        <xsl:attribute name="class"><xsl:value-of select="concat('tee-',@colour)"/></xsl:attribute>
                        <xsl:element name="td">
                                <xsl:attribute name="bgcolor">
                                        <xsl:value-of select="@colour"/>
                                </xsl:attribute>
                        </xsl:element>
                        <td>
                                <xsl:value-of select="g:qualification/g:qualification-usga/g:rating"/>
                        </td>
                        <td>
                                <xsl:value-of select="g:qualification/g:qualification-usga/g:slope"/>
                        </td>
                        <xsl:for-each select="g:tee">
                                <xsl:sort select="@number" data-type="number"/>
                                <td>
                                        <xsl:value-of select="g:length[@units='meters']"/>
                                </td>
                        </xsl:for-each>
                        
                        <td><xsl:value-of select="sum(g:tee[@number &lt; 10]/g:length[@units='meters'])"/></td>
                        <td><xsl:value-of select="sum(g:tee[@number > 9]/g:length[@units='meters'])"/></td>
                        <td><xsl:value-of select="sum(g:tee/g:length[@units='meters'])"/></td>
                </xsl:element>
                <tr class="par">
                        <td colspan="3" class="label">Par</td> 
                        <xsl:for-each select="g:tee">
                                <xsl:sort select="@number" data-type="number"/>
                                <td>
                                        <xsl:value-of select="g:par"/>
                                </td>
                        </xsl:for-each>
                        <td><xsl:value-of select="sum(g:tee[@number &lt; 10]/g:par)"/></td>
                        <td><xsl:value-of select="sum(g:tee[@number > 9]/g:par)"/></td>
                        <td><xsl:value-of select="sum(g:tee/g:par)"/></td>
                </tr>   
                <tr class="par">
                        <td colspan="3" class="label">Allowed</td> 
                        <xsl:for-each select="g:tee">
                                <xsl:sort select="@number" data-type="number"/>
                                <td>
                                        <xsl:if test="g:handicap-stroke > $holes-some">
                                                <xsl:value-of select="$holes-all"/>
                                        </xsl:if>
                                        <xsl:if test="g:handicap-stroke &lt;= $holes-some">
                                                <xsl:value-of select="$holes-all + 1"/>
                                        </xsl:if>
                                </td>
                        </xsl:for-each>
                        <td></td>
                        <td></td>
                        <td><xsl:value-of select="$strokes"/></td>
                </tr>
                <tr class="par">
                        <td colspan="3" class="label">Total</td> 
                        <xsl:for-each select="g:tee">
                                <xsl:sort select="@number" data-type="number"/>
                                <td>
                                        <xsl:if test="g:handicap-stroke > $holes-some">
                                                <xsl:value-of select="g:par + $holes-all"/>
                                        </xsl:if>
                                        <xsl:if test="g:handicap-stroke &lt;= $holes-some">
                                                <xsl:value-of select="g:par + $holes-all + 1"/>
                                        </xsl:if>
                                </td>
                        </xsl:for-each>
                        <td></td>
                        <td></td>
                        <td><xsl:value-of select="sum(g:tee/g:par) + $strokes"/></td>
                </tr>
                
                <xsl:call-template name="EmptyLine">
                        <xsl:with-param name="label">Fairway</xsl:with-param>
                </xsl:call-template>
                <xsl:call-template name="EmptyLine">
                        <xsl:with-param name="label">Drive</xsl:with-param>
                </xsl:call-template>
                <xsl:call-template name="EmptyLine">
                        <xsl:with-param name="label">GIR</xsl:with-param>
                </xsl:call-template>
                <xsl:call-template name="EmptyLine">
                        <xsl:with-param name="label">First putt</xsl:with-param>
                </xsl:call-template>
                <xsl:call-template name="EmptyLine">
                        <xsl:with-param name="label">Putt</xsl:with-param>
                </xsl:call-template>
                <xsl:call-template name="EmptyLine">
                        <xsl:with-param name="label">Strokes</xsl:with-param>
                </xsl:call-template>
                
                <xsl:if test="/g:golfml/g:application[@dotname='com.personalpar']">
                        <tr>
                                <td colspan="3">Personal par</td> 
                                <xsl:for-each select="g:tee">
                                        <xsl:sort select="@number" data-type="number"/>
                                        <td>
                                                <xsl:call-template name="PersonalPar">
                                                        <xsl:with-param name="len"><xsl:value-of select="number(g:length[@units='meters'])"/></xsl:with-param>
                                                </xsl:call-template>            
                                        </td>
                                </xsl:for-each>
                                <td></td>
                                <td></td>
                                <td></td>
                        </tr>
                </xsl:if>
        </xsl:template>
        
        
        <xsl:template name="PersonalPar">
                <xsl:param name="len"/>
                <xsl:choose><!-- men's values -->
                        <xsl:when test="$len &lt; 166">3</xsl:when>
                        <xsl:when test="$len > 165 and $len &lt; 351">4</xsl:when>
                        <xsl:when test="$len > 350 and $len &lt; 451">5</xsl:when>
                        <xsl:when test="$len > 450 and $len &lt; 551">6</xsl:when>
                        <xsl:otherwise>7</xsl:otherwise>
                </xsl:choose>           
        </xsl:template>
        

        <xsl:template name="EmptyLine">
                <xsl:param name="label"/>
                <tr>
                        <td colspan="3" class="label"><xsl:value-of select="$label"/></td> 
                        <xsl:for-each select="g:tee">
                                <xsl:sort select="@number" data-type="number"/>
                                <td>
                                </td>
                        </xsl:for-each>
                        <td></td>
                        <td></td>
                        <td></td>
                </tr>   
        </xsl:template>
        
</xsl:stylesheet>