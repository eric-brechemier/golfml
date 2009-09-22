<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	version="1.0"
	xmlns:g="http://code.google.com/p/golfml"
	>
	<!-- simple-score.xsl
		
		DESCRIPTION
		
		Demonstration stylesheet for GolfML.
		Generate a simple score presentation for a round of one to 4 players.		
		
		
		VERSION
		$Revision$
		
		
		HISTORY
		Aug 2009: Created with MGS Version 1.40.
		
	-->
	
	<xsl:output method="html" indent="yes"/>
	
	<xsl:template match="g:golfml">
		<html>
			<body>
				<xsl:apply-templates select="g:player"/>
				<hr/>
			</body>
		</html>
	</xsl:template>
	
	<xsl:template match="g:player">
		<h1>Player: <xsl:value-of select="g:name"/></h1>
		<p>Handicap: <xsl:value-of select="g:round/g:scorecard/g:handicap"/></p>
		<xsl:apply-templates select="g:round"/>
	</xsl:template>
	
	<xsl:template match="g:round">
		<h2>Date: <xsl:value-of select="g:date"/></h2>
		<xsl:if test="g:weather">
			<p> Weather: <xsl:value-of select="g:weather"/>. (Wind: <xsl:value-of select="g:weather/@wind"/>) </p>
		</xsl:if>
		<xsl:apply-templates select="g:scorecard"/>
	</xsl:template>
	
	<xsl:template match="g:scorecard">
		
		<h3>Course: <xsl:value-of select="g:tees/g:country-club.golf-course.name"/></h3>
		<p>Golf club: <xsl:value-of select="g:tees/g:country-club.name"/></p>
		<table border="1">
			<xsl:call-template name="Tees">
				<xsl:with-param name="country"
					select="g:tees/g:country-club.address.country.iso3166"/>
				<xsl:with-param name="city" select="g:tees/g:country-club.postal-code"/>
				<xsl:with-param name="gcc" select="g:tees/g:country-club.name"/>
				<xsl:with-param name="course" select="g:tees/g:country-club.golf-course.name"/>
				<xsl:with-param name="tee" select="g:tees/g:country-club.golf-course.tee-set.name"/>
			</xsl:call-template>
			<tbody>
				<xsl:apply-templates select="g:score"/>
			</tbody>
		</table>
		
	</xsl:template>
	
	<xsl:template name="Tees">
		<xsl:param name="country"/>
		<xsl:param name="city"/>
		<xsl:param name="gcc"/>
		<xsl:param name="course"/>
		<xsl:param name="tee"/>
		<thead>
			<tr>
				<td> </td>
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
		<xsl:apply-templates
			select="/g:golfml/g:country-club[g:name=$gcc]/g:golf-course[g:name=$course]/g:tee-set[@name=$tee]"/>
	</xsl:template>
	
	
	<xsl:template match="g:country-club">
		<b>
			<i>
				<xsl:value-of select="g:name"/>
			</i>
		</b>
	</xsl:template>
	
	<xsl:template match="g:tee-set">
		<tr>
			<td colspan="4">Par</td>
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
			<td colspan="4">Handicap</td>
			<xsl:for-each select="g:tee">
				<td>
					<xsl:value-of select="g:handicap-stroke"/>
				</td>
			</xsl:for-each>
		</tr>
		
		<tr>
			<xsl:element name="td">
				<xsl:attribute name="bgcolor">
					<xsl:value-of select="@colour"/>
				</xsl:attribute>
			</xsl:element>
			<td>
				<xsl:value-of select="@colour"/>
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
				<xsl:value-of select="sum(g:tee[@number &lt; 10]/g:length)"/>
			</td>
			<td>
				<xsl:value-of select="sum(g:tee[@number > 9]/g:length)"/>
			</td>
			<td>
				<xsl:value-of select="sum(g:tee/g:length)"/>
			</td>
		</tr>
		
		
	</xsl:template>
	
	<xsl:template match="g:score">
		<tr>
			<td colspan="4"> Strokes </td>
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
		
		<tr>
			<td colspan="4">Putts</td>
			<xsl:for-each select="g:hole">
				<xsl:sort select="@number"/>
				<td>
					<xsl:value-of select="g:putts"/>
				</td>
			</xsl:for-each>
			<td>
				<xsl:value-of select="sum(g:hole[@number &lt; 10]/g:putts)"/>
			</td>
			<td>
				<xsl:value-of select="sum(g:hole[@number > 9]/g:putts)"/>
			</td>
			<td>
				<xsl:value-of select="sum(g:hole/g:putts)"/>
			</td>
		</tr>
		
	</xsl:template>
	
</xsl:stylesheet>