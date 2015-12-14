<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="2.0"
				xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
				xmlns:g="http://code.google.com/p/golfml"
	>
<!-- score.xsl

	DESCRIPTION
	
	Demonstration stylesheet for GolfML.
	Generate a compact score presentation for a round of one to 4 players.
	
	
	VERSION
	$Revision$
	
	
	HISTORY
	Aug 2009: Created with MGS Version 1.40.
	
-->
	
	<xsl:output method="html" indent="yes"/>

	<xsl:param name="round-date">2009-09-22T11:00:00</xsl:param>
	
	<xsl:template match="g:golfml">
		<html>
			<head>
				<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
				<link href="../../stylesheets/golfml.css" rel="stylesheet" type="text/css"/>
				<title>GolfML Scorecard</title>
			</head>
			<body><!-- /default:golfml/default:player[1]/default:round[1]/default:date[1] -->
				<xsl:apply-templates select="//g:round[g:date=$round-date]"/>
				<xsl:value-of disable-output-escaping="yes" select="concat('&lt;','/table')"/>
			</body>
		</html>
	</xsl:template>

	<xsl:template match="g:round">
		<xsl:if test="position() = 1">
			<h1>Scorecard</h1>
			<xsl:if test="g:weather">
				<p>Weather: <xsl:value-of select="g:weather"/>. (Wind: <xsl:value-of select="g:weather/@wind"/>) </p>
			</xsl:if>
			<xsl:value-of disable-output-escaping="yes" select="concat('&lt;','table class=&quot;course-data&quot;>')"/>
			<caption>
				<xsl:value-of select="g:scorecard/g:tees/g:country-club.name"/>(<xsl:value-of select="g:scorecard/g:tees/g:country-club.golf-course.name"/>)
				Date: <xsl:value-of select="format-dateTime(g:date, '[D] [MNn] [Y]', 'fr', (), ())"/>
			</caption>

			<xsl:variable name="gcc"><xsl:value-of select="g:scorecard/g:tees/g:country-club.name"/></xsl:variable>
			<xsl:variable name="course"><xsl:value-of select="g:scorecard/g:tees/g:country-club.golf-course.name"/></xsl:variable>
			<xsl:apply-templates select="/g:golfml/g:country-club[g:name=$gcc]/g:golf-course[g:name=$course]/g:tee-set"/>
		</xsl:if>
		<xsl:apply-templates select="g:scorecard/g:score"/>
		<!-- xsl:value-of select="parent::node()/g:name"/ -->
	</xsl:template>

	<xsl:template match="g:tee-set">
		<xsl:if test="position() = 1">
			<thead>
				<tr>
					<td>Tee Color</td>
					<td>Rating</td>
					<td>Slope</td>
					<td>1</td>
					<td>2</td>
					<td>3</td>
					<td>4</td>
					<td>5</td>
					<td>6</td>
					<td>7</td>
					<td>8</td>
					<td>9</td>
					<td>10</td>
					<td>11</td>
					<td>12</td>
					<td>13</td>
					<td>14</td>
					<td>15</td>
					<td>16</td>
					<td>17</td>
					<td>18</td>
					<td>Out</td>
					<td>In</td>
					<td>Total</td>
				</tr>
			</thead>
			<tr>
				<td colspan="3" class="label">Par</td>
				<xsl:for-each select="g:tee">
					<td>
						<xsl:value-of select="g:par"/>
					</td>
				</xsl:for-each>
				<td>
					<xsl:value-of select="sum(g:tee[@number &lt; 10]/g:par)"/>
				</td>
				<td>
					<xsl:value-of select="sum(g:tee[@number > 9]/g:par)"/>
				</td>
				<td>
					<xsl:value-of select="sum(g:tee/g:par)"/>
				</td>
			</tr>
			<tr>
				<td colspan="3" class="label">Handicap</td>
				<xsl:for-each select="g:tee">
					<td>
						<xsl:value-of select="g:handicap-stroke"/>
					</td>
				</xsl:for-each>
			</tr>
		</xsl:if>
		
		
		<xsl:element name="tr">
			<xsl:if test="@colour">
				<xsl:attribute name="class"><xsl:value-of select="concat('tee-',@colour)"/></xsl:attribute>
			</xsl:if>

			<td>
				<xsl:value-of select="@name"/>
			</td>
			<td>
				<xsl:value-of select="g:qualification/g:qualification-usga/g:rating"/>
			</td>
			<td>
				<xsl:value-of select="g:qualification/g:qualification-usga/g:slope"/>
			</td>
			<xsl:for-each select="g:tee">
				<xsl:sort select="@number"/>
				<td>
					<xsl:value-of select="g:length"/>
				</td>
			</xsl:for-each>
			
			<td>
				<xsl:value-of select="sum(g:tee[@number &lt; 10]/g:length[@units = 'meters'])"/>
			</td>
			<td>
				<xsl:value-of select="sum(g:tee[@number > 9]/g:length[@units = 'meters'])"/>
			</td>
			<td>
				<xsl:value-of select="sum(g:tee/g:length[@units = 'meters'])"/>
			</td>
		</xsl:element>
	</xsl:template>


	<xsl:template match="g:score">
		<tr>
			<td><xsl:value-of select="../../../g:name"/></td>
			<td><xsl:value-of select="../g:tees/g:country-club.golf-course.tee-set.name"/></td>
			<td class="label">Strokes</td>
			<xsl:for-each select="g:hole">
				<xsl:sort select="@number"/>
				<td>
					<xsl:value-of select="g:strokes"/>
				</td>
			</xsl:for-each>
			<td>
				<xsl:value-of select="sum(g:hole[@number &lt; 10]/g:strokes)"/>
			</td>
			<td>
				<xsl:value-of select="sum(g:hole[@number > 9]/g:strokes)"/>
			</td>
			<td>
				<xsl:value-of select="sum(g:hole/g:strokes)"/>
			</td>
		</tr>
	</xsl:template>

</xsl:stylesheet>
