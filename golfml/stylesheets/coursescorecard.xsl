<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0"
                                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                                xmlns:g="http://code.google.com/p/golfml" >
<!-- static-course.xsl

DESCRIPTION

        Demonstration stylesheet for GolfML.
        Generate a simple score card from a golf course description.            


VERSION
        $Revision$


HISTORY
        Aug 2009: Created.
-->
        <xsl:output method="html" indent="yes" encoding="UTF-8"/>
        
        <xsl:param name="one-par-line-per-gender">true</xsl:param>
        <xsl:param name="units">meters</xsl:param><!-- meters|yards -->

        <xsl:template match="g:golfml">
                <html>
                        <head>
                                <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
                                <link href="golfml.css" rel="stylesheet" type="text/css"/>
                                <title>GolfML Scorecard</title>
                        </head>
                        <body>
                                <xsl:apply-templates select="g:country-club"/>
                        </body>
                </html>
        </xsl:template>

        <xsl:template match="g:country-club">
                <h1><xsl:value-of select="g:name"/></h1>
                <p align="center">
                <xsl:apply-templates select="g:address"/>
       			<br/>Phone: <xsl:apply-templates select="g:contact"/>
       			</p>
                <hr/>
                <xsl:apply-templates select="g:golf-course"/>
        </xsl:template>

        <xsl:template match="g:address">
        		<xsl:value-of select="g:street"/>,
        		<xsl:value-of select="g:municipality"/>,
        		<xsl:value-of select="g:region"/>,
        		<xsl:value-of select="g:country "/>
        		<br/>
       		Website: <xsl:element name="a">
                          <xsl:attribute name="href"><xsl:value-of select="g:website"/></xsl:attribute>
                          <xsl:value-of select="g:website"/>
                     </xsl:element>
	</xsl:template>

        <xsl:template match="g:contact">
        	<xsl:value-of select="g:phone"/>
	</xsl:template>
		
        <xsl:template match="g:golf-course">
                <table class="course-data">
                        <caption><xsl:value-of select="g:name"/></caption>
                        <thead>
                                <tr id="header">
                                        <td>Tee Colour</td>
                                        <td>Rating</td>
                                        <td>Slope</td>
                                        <xsl:for-each select="g:tee-set[position() = 1]/g:tee">
                                                <xsl:sort select="@number" data-type="number"/>
                                                <td>                                            
                                                        <xsl:value-of select="position()"/>
                                                </td>
                                        </xsl:for-each>
                                        <td>Out</td>
                                        <td>In</td>
                                        <td>Total</td>
                                </tr>
                        </thead>
                        <tbody>
                                <xsl:if test="g:tee-set[@gender='ladies']">
                                        <tr id="title-ladies"><td colspan="24">Ladies</td></tr>
                                        <xsl:for-each select="g:tee-set[@gender='ladies']">
                                                <xsl:if test="position() = 1">
                                                        <tr class="par">
                                                                <td colspan="3" class="label">Par</td> 
                                                                <xsl:for-each select="g:tee">
                                                                        <td>
                                                                                <xsl:value-of select="g:par"/>
                                                                        </td>
                                                                </xsl:for-each>
                                                                <td><xsl:value-of select="sum(g:tee[@number &lt; 10]/g:par)"/></td>
                                                                <td><xsl:value-of select="sum(g:tee[@number > 9]/g:par)"/></td>
                                                                <td><xsl:value-of select="sum(g:tee/g:par)"/></td>
                                                        </tr>
                                                        
                                                        <tr class="handicap">
                                                                <td colspan="3" class="label">Handicap</td> 
                                                                <xsl:for-each select="g:tee">
                                                                        <td>
                                                                                <xsl:value-of select="g:handicap-stroke"/>
                                                                        </td>
                                                                </xsl:for-each>
                                                        </tr>
                                                </xsl:if>
                                                <xsl:apply-templates select="."/>
                                        </xsl:for-each>
                                </xsl:if>
                                <xsl:if test="g:tee-set[@gender='gentlemen']">
                                        <tr id="title-gentlemen"><td colspan="24">Gentlemen</td></tr>
                                        <xsl:for-each select="g:tee-set[@gender='gentlemen']">
                                                <xsl:if test="position() = 1">
                                                        <tr class="par">
                                                                <td colspan="3" class="label">Par</td> 
                                                                <xsl:for-each select="g:tee">
                                                                        <td>
                                                                                <xsl:value-of select="g:par"/>
                                                                        </td>
                                                                </xsl:for-each>
                                                                <td><xsl:value-of select="sum(g:tee[@number &lt; 10]/g:par)"/></td>
                                                                <td><xsl:value-of select="sum(g:tee[@number > 9]/g:par)"/></td>
                                                                <td><xsl:value-of select="sum(g:tee/g:par)"/></td>
                                                        </tr>
                                                        
                                                        <tr class="handicap">
                                                                <td colspan="3" class="label">Handicap</td> 
                                                                <xsl:for-each select="g:tee">
                                                                        <td>
                                                                                <xsl:value-of select="g:handicap-stroke"/>
                                                                        </td>
                                                                </xsl:for-each>
                                                        </tr>
                                                </xsl:if>
                                                <xsl:apply-templates select="."/>
                                        </xsl:for-each>
                                </xsl:if>
                        </tbody>
                </table>
                <p style="text-align: center;">
                Note: Distance in <xsl:value-of select="$units"/>.
                </p>
                <hr />
        </xsl:template>
        
        
        <xsl:template match="g:hole">
                <td>
                        <xsl:choose>
                                <xsl:when test="g:media">
                                        <xsl:element name="a">
                                                <xsl:attribute name="href"><xsl:value-of select="g:media/g:url"/></xsl:attribute>
                                                <xsl:attribute name="alt"><xsl:value-of select="g:media/g:name"/> (<xsl:value-of select="g:media/@type"/>)</xsl:attribute>
                                                <xsl:value-of select="@number"/>
                                        </xsl:element>
                                </xsl:when>
                                <xsl:otherwise>
                                        <xsl:choose>
                                                <xsl:when test="g:placemarks">
                                                        <xsl:element name="a">
                                                                <xsl:attribute name="href"><xsl:value-of select="concat('hole-',@number,'.svg')"/></xsl:attribute>
                                                                <xsl:attribute name="alt"><xsl:value-of select="concat('Drawing of hole ', @number)"/></xsl:attribute>
                                                                <xsl:attribute name="title"><xsl:value-of select="concat('Drawing of hole ', @number)"/></xsl:attribute>
                                                                <xsl:attribute name="target">_blank</xsl:attribute>
                                                                <xsl:value-of select="@number"/>
                                                        </xsl:element>
                                                </xsl:when>
                                                <xsl:otherwise>
                                                        <xsl:value-of select="@number"/>
                                                </xsl:otherwise>
                                        </xsl:choose>                                                                           
                                </xsl:otherwise>
                        </xsl:choose>
                        
                </td>
        </xsl:template>


        <xsl:template match="g:tee-set">
                <xsl:element name="tr">
                        <xsl:attribute name="class"><xsl:value-of select="concat('tee-',@colour)"/></xsl:attribute>
                        <td class="label">
                                <xsl:value-of select="@colour"/>
                        </td>
                        <td>
                                <xsl:value-of select="g:qualification/g:qualification-usga/g:rating"/>
                        </td>
                        <td>
                                <xsl:value-of select="g:qualification/g:qualification-usga/g:slope"/>
                        </td>
                        <xsl:for-each select="g:tee">
                                <xsl:sort select="@number" data-type="number"/>
                                <td>
                                        <xsl:choose>
                                                <xsl:when test="g:length/@units">
                                                        <xsl:value-of select="g:length[@units=$units]"/>
                                                </xsl:when>
                                                <xsl:otherwise>
                                                        <xsl:value-of select="g:length"/>
                                                </xsl:otherwise>
                                        </xsl:choose>
                                </td>
                        </xsl:for-each>
                        
                        <xsl:choose><!-- need to refine here for missing attribute (default is meters) -->
                                <xsl:when test="$units = 'meters'">
                                        <xsl:choose><!-- if first one has units attribute, we assume they all have... -->
                                                <xsl:when test="g:tee[1]/g:length[@units = 'meters'] > 0">
                                                        <td><xsl:value-of select="sum(g:tee[@number &lt; 10]/g:length[@units=$units])"/></td>
                                                        <td><xsl:value-of select="sum(g:tee[@number > 9]/g:length[@units=$units])"/></td>
                                                        <td><xsl:value-of select="sum(g:tee/g:length[@units=$units])"/></td>
                                                </xsl:when>
                                                <xsl:otherwise>
                                                        <td><xsl:value-of select="sum(g:tee[@number &lt; 10]/g:length)"/></td>
                                                        <td><xsl:value-of select="sum(g:tee[@number > 9]/g:length)"/></td>
                                                        <td><xsl:value-of select="sum(g:tee/g:length)"/></td>
                                                </xsl:otherwise>
                                        </xsl:choose>
                                </xsl:when>
                                <xsl:otherwise>
                                        <td><xsl:value-of select="sum(g:tee[@number &lt; 10]/g:length[@units=$units])"/></td>
                                        <td><xsl:value-of select="sum(g:tee[@number > 9]/g:length[@units=$units])"/></td>
                                        <td><xsl:value-of select="sum(g:tee/g:length[@units=$units])"/></td>
                                </xsl:otherwise>
                        </xsl:choose>
                        
                </xsl:element>
        </xsl:template>

</xsl:stylesheet>