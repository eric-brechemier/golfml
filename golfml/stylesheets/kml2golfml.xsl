<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0"
	xmlns="http://code.google.com/p/golfml"
	xmlns:kml="http://www.opengis.net/kml/2.2"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<!-- kml2golfml.xsl
    
    DESCRIPTION
    
    	Transformation sheet for kml structured document to golfml hole description.
     
      
	VERSION
     	$Revision$
     	
     	
	HISTORY
    	Sep 2009: Created.
     	
	-->
	
	<xsl:output method="xml" version="1.0" indent="yes"/>

	<xsl:template match="/kml:kml/kml:Document">
		<xsl:element name="golfml">

			<xsl:attribute name="xmlns:xsi">http://www.w3.org/2001/XMLSchema-instance</xsl:attribute>
			<xsl:attribute name="xsi:schemaLocation">http://code.google.com/p/golfml ../schemas/golfml1.xsd</xsl:attribute>
			<xsl:attribute name="version">0.9</xsl:attribute>
			
			<xsl:apply-templates select="kml:Folder" mode="golf-club"/>
			
		</xsl:element>
	</xsl:template>



	<xsl:template match="kml:Folder" mode="golf-club">
		<xsl:element name="country-club">
			<xsl:element name="name"><xsl:value-of select="kml:name"/></xsl:element>
			<xsl:element name="address">
				<xsl:element name="country">
					<xsl:attribute name="code">US</xsl:attribute>
				</xsl:element>
				<xsl:element name="postal-code">0</xsl:element>
			</xsl:element>
			<xsl:for-each select="kml:Folder">
				<xsl:element name="golf-course">
					<xsl:element name="name"><xsl:value-of select="kml:name"/></xsl:element>
					<xsl:element name="holes">
						<xsl:for-each select="kml:Folder">
							<xsl:element name="hole">
								<xsl:attribute name="number"><xsl:value-of select="substring-after(kml:name, ' ')"/></xsl:attribute>
								<xsl:attribute name="name"><xsl:value-of select="kml:name"/></xsl:attribute>
								<xsl:element name="placemarks">
									<xsl:apply-templates select="kml:Placemark"/>
								</xsl:element>
							</xsl:element><!--hole-->							
						</xsl:for-each>
					</xsl:element><!--holes-->
				</xsl:element><!--golf-course-->
			</xsl:for-each>
		</xsl:element><!--country-club-->
	</xsl:template>
	
	
	
	<xsl:template match="kml:Folder" mode="hole">
		
	</xsl:template>



	<xsl:template match="kml:Placemark">
		
		<xsl:if test="kml:Point">
			<xsl:element name="poi">
				<xsl:attribute name="type">
					<xsl:call-template name="TypeFinder">
						<xsl:with-param name="name"><xsl:value-of select="translate(kml:name,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz')"/></xsl:with-param>
					</xsl:call-template>
				</xsl:attribute>
				<xsl:element name="name"><xsl:value-of select="kml:name"/></xsl:element>
				<xsl:element name="description"><xsl:value-of select="kml:description"/></xsl:element>
				<xsl:element name="position">
					<xsl:call-template name="SplitCoordinate">
						<xsl:with-param name="coordinate" select="kml:Point"/>
						<xsl:with-param name="separator">,</xsl:with-param> <!-- sometimes ,,? -->
					</xsl:call-template>
				</xsl:element>
			</xsl:element>
		</xsl:if>
		
		<xsl:if test="kml:Polygon">
			<xsl:element name="aoi">
				<xsl:attribute name="type">
					<xsl:call-template name="TypeFinder">
						<xsl:with-param name="name"><xsl:value-of select="translate(kml:name,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz')"/></xsl:with-param>
					</xsl:call-template>
				</xsl:attribute>
				<xsl:element name="name"><xsl:value-of select="kml:name"/></xsl:element>
				<xsl:element name="description"><xsl:value-of select="kml:description"/></xsl:element>
				<xsl:call-template name="SplitCoordinates" >
					<xsl:with-param name="coordinates" select=".//kml:coordinates" />
					<xsl:with-param name="counter" select="1"/>
				</xsl:call-template>
			</xsl:element>
		</xsl:if>
		
	</xsl:template>
	
	
	
	<xsl:template name="SplitCoordinates">
		<!-- splits a set of coordinates triplets from a single (long) string
			 separator between triplets is a space.
		-->
		
		<xsl:param name="coordinates"/>
		<xsl:param name="counter"/>
		
		<xsl:element name="position">
			<xsl:attribute name="number"><xsl:value-of select="$counter"/></xsl:attribute>
			<xsl:call-template name="SplitCoordinate">
				<xsl:with-param name="coordinate" select="substring-before($coordinates, ' ')"/>
				<xsl:with-param name="separator">,</xsl:with-param>
			</xsl:call-template>
		</xsl:element>

		<xsl:if test="string-length(substring-after($coordinates, ' '))> 2"> <!-- CHECK TEST -->
			<xsl:call-template name="SplitCoordinates">
				<xsl:with-param name="coordinates" select="substring-after($coordinates, ' ')"/>
				<xsl:with-param name="counter"><xsl:value-of select="$counter + 1"/></xsl:with-param>
			</xsl:call-template>
		</xsl:if>

	</xsl:template>
	
	
	
	<xsl:template name="SplitCoordinate">
		<!-- splits one set of coordinate (longitude,latitude,altitude)
			 separator varies.
		  -->
		<xsl:param name="coordinate"/>
		<xsl:param name="separator"/>
		<xsl:element name="position-gps">
			<xsl:element name="lon"><xsl:value-of select="substring-before($coordinate, $separator)"/></xsl:element>
			<xsl:element name="lat"><xsl:value-of select="substring-before(substring-after($coordinate, $separator), $separator)"/></xsl:element>
			<xsl:element name="alt"><xsl:value-of select="substring-after(substring-after($coordinate, $separator), $separator)"/></xsl:element>
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
