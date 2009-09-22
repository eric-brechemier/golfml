<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	version="1.0"
	xmlns:g="http://code.google.com/p/golfml"
	>
	<!-- static-course.xsl
	
	DESCRIPTION
	
		Demonstration stylesheet for GolfML.
		Generate a simple score card from a golf course description.		
	
	
	VERSION
		$Revision$
	
	
	HISTORY
		Aug 2009: Created with MGS Version 1.40.
	-->
	<xsl:output method="html" indent="yes"/>

	<xsl:template match="g:golfml">
		<html>
			<body>
				<xsl:apply-templates select="g:country-club"/>
			</body>
		</html>
	</xsl:template>

	<xsl:template match="g:country-club">
		<h1>Country club:<xsl:value-of select="g:name"/></h1>
		<xsl:apply-templates select="g:golf-course"/>
	</xsl:template>

	<xsl:template match="g:golf-course">
		<h2>Course: <xsl:value-of select="g:name"/></h2>
		<table border="1">
			<thead>
				<tr>
					<td> </td>
					<td>Tee Color</td>
					<td>Rating</td>
					<td>Slope <xsl:value-of select="count(g:holes)"/></td>
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
				
				<tr><td colspan="24">Gentlemen</td></tr>
				<xsl:apply-templates select="g:tee-set[@gender='gentlemen']"/>
				<tr><td colspan="24">Women</td></tr>
				<xsl:apply-templates select="g:tee-set[@gender='ladies']"/>				

			</tbody>
		</table>
		<hr />
	</xsl:template>

	<xsl:template match="g:tee-set">
		<tr>
			<td colspan="4">Par</td> 
			<xsl:for-each select="g:tee">
				<td>
					<xsl:value-of select="g:par"/>
				</td>
			</xsl:for-each>
			<td><xsl:value-of select="sum(g:tee[@number &lt; 10]/g:par)"/></td>
			<td><xsl:value-of select="sum(g:tee[@number > 9]/g:par)"/></td>
			<td><xsl:value-of select="sum(g:tee/g:par)"/></td>
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
					<xsl:value-of select="@color"/>
				</xsl:attribute>
			</xsl:element>
			<td>
				<xsl:value-of select="@color"/>
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
			
			<td><xsl:value-of select="sum(g:tee[@number &lt; 10]/g:length)"/></td>
			<td><xsl:value-of select="sum(g:tee[@number > 9]/g:length)"/></td>
			<td><xsl:value-of select="sum(g:tee/g:length)"/></td>
		</tr>
	</xsl:template>

</xsl:stylesheet>
