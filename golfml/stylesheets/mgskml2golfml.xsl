<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0"
	xmlns="http://code.google.com/p/golfml"
	xmlns:kml="http://earth.google.com/kml/2.0"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<!-- mgskml2golfml.xsl
    
    DESCRIPTION
    
    Mobile Golf Scorer course positions to GolfML transformation style sheet
     
      
     VERSION
     	$Revision$
     	
     HISTORY
     	Aug 2009: Created with MGS Version 1.40.
     	
-->

	<xsl:template match="/kml:kml/kml:Folder">
		<xsl:processing-instruction name="xml-stylesheet">
			href="../stylesheets/golfml2svg.xsl" type="text/xsl"
		</xsl:processing-instruction>
		<xsl:element name="golfml">
			<xsl:attribute name="xmlns:xsi">http://www.w3.org/2001/XMLSchema-instance</xsl:attribute>
			<xsl:attribute name="xsi:schemaLocation">http://code.google.com/p/golfml file:/Users/pierre/Developer/iGolf/golfml/schemas/golfml1.xsd</xsl:attribute>
			<xsl:attribute name="version">0.9</xsl:attribute>

			<xsl:element name="country-club">
				<xsl:element name="name"><xsl:value-of select="kml:name"/></xsl:element>
				<xsl:element name="address">
					<xsl:element name="country">
						<xsl:attribute name="code">US</xsl:attribute>
					</xsl:element>
					<xsl:element name="postal-code">0</xsl:element>
				</xsl:element>
				<xsl:element name="golf-course">
					<xsl:element name="name"><xsl:value-of select="kml:name"/></xsl:element>
					<xsl:element name="holes">
						<xsl:for-each select="kml:Folder">
							<xsl:element name="hole">
								<xsl:attribute name="number"><xsl:value-of select="position()"/></xsl:attribute>
								<xsl:attribute name="name"><xsl:value-of select="kml:name"/></xsl:attribute>
								<xsl:element name="placemarks">
									<xsl:apply-templates select="kml:Placemark"/>
								</xsl:element>
							</xsl:element>							
						</xsl:for-each>
					</xsl:element><!--holes-->
				</xsl:element><!--golf-course-->
			</xsl:element><!--country-club-->
		</xsl:element>
	</xsl:template>
	

	<xsl:template match="kml:Placemark">
		<xsl:element name="poi">
			<xsl:attribute name="type">
				<xsl:call-template name="TypeFinder">
					<xsl:with-param name="name"><xsl:value-of select="translate(kml:name,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz')"/></xsl:with-param>
				</xsl:call-template>
			</xsl:attribute>
			<xsl:element name="name"><xsl:value-of select="kml:name"/></xsl:element>
			<xsl:element name="description"><xsl:value-of select="kml:description"/></xsl:element>
			<xsl:element name="position">
				<xsl:element name="position-gps">
					<xsl:element name="lon"><xsl:value-of select="substring-before(kml:Point, ',')"/></xsl:element>
					<xsl:element name="lat"><xsl:value-of select="substring-before(substring-after(kml:Point, ','), ',')"/></xsl:element>
					<xsl:element name="alt"><xsl:value-of select="substring-after(substring-after(kml:Point, ','), ',')"/></xsl:element>
				</xsl:element>
			</xsl:element>
		</xsl:element>
	</xsl:template>
	
	
	<xsl:template name="TypeFinder">
		<xsl:param name="name"/>
		
		<xsl:choose>
			<xsl:when test="contains($name, 'tee')">tee</xsl:when>
			<xsl:when test="contains($name, 'fairway')">fairway</xsl:when>
			<xsl:when test="contains($name, 'green')">green</xsl:when>
			<xsl:when test="contains($name, 'fringe')">fringe</xsl:when>
			<xsl:when test="contains($name, 'tree')">tree</xsl:when>
			<xsl:when test="contains($name, 'trees')">trees</xsl:when>
			<xsl:when test="contains($name, 'bush')">bush</xsl:when>
			<xsl:when test="contains($name, 'semi-rough')">semi-rough</xsl:when>
			<xsl:when test="contains($name, 'rough')">rough</xsl:when>
			<xsl:when test="contains($name, 'heavy-rough')">heavy-rough</xsl:when>
			<xsl:when test="contains($name, 'trap')">trap</xsl:when>
			<xsl:when test="contains($name, 'bunker')">trap</xsl:when>
			<xsl:when test="contains($name, 'greenside-trap')">greenside-trap</xsl:when>
			<xsl:when test="contains($name, 'fairway-trap')">fairway-trap</xsl:when>
			<xsl:when test="contains($name, 'front-water')">front-water</xsl:when>
			<xsl:when test="contains($name, 'water')">water</xsl:when>
			<xsl:when test="contains($name, 'lateral-water')">lateral-water</xsl:when>
			<xsl:when test="contains($name, 'path')">path</xsl:when>
			<xsl:when test="contains($name, 'building')">building</xsl:when>
			<xsl:when test="contains($name, 'construction')">building</xsl:when>
			<xsl:when test="contains($name, 'obstruction')">obstruction</xsl:when>
			<xsl:when test="contains($name, 'out-of-bound')">out-of-bound</xsl:when>
			<xsl:when test="contains($name, 'oob')">out-of-bound</xsl:when>
			<xsl:when test="contains($name, 'contour')">hole-contour</xsl:when>
			<xsl:when test="contains($name, 'aim')">aim</xsl:when>
			<xsl:when test="contains($name, 'dogleg')">aim</xsl:when>
			<xsl:otherwise>other</xsl:otherwise>
		</xsl:choose>
		
	</xsl:template>
	
	
	


</xsl:stylesheet>
