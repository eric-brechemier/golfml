<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0"
	xmlns="http://code.google.com/p/golfml"
	xmlns:kml="http://www.opengis.net/kml/2.2"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<!-- kml2golfml.xsl
	
	DESCRIPTION
	
	Transformation sheet for kml structured document to golfml hole description.
	Structure of original document should be something like this:
	|
	+- Country Club Name			<<== FOLDER LEVEL ONE: country-club
	|   |
	+- Golf Course Name				<<== FOLDER LEVEL TWO: golf-course
	|     |
	|     +- Facilities				<<== FOLDER LEVEL THREE A: facilities: Things not related to a hole.
	|     |    |
	|     |    +O Club house
	|     |    +O Practice area
	|     |    ...
	|     |    |
	|     +- Hole 1					<<== FOLDER LEVEL THREE B: hole. Hole name should contain the number of the hole.
	|     |    |
	|     |    +O Green				<<== Name of polygons or points is important: It matches valid area/point of interest type in golfml.
	|     |    +O Tee Black
	|     |    +O Fairway
	|     |    +O Green-side bunker
	|     |    +O Tree
	|     |       ...
	|     +- Hole 2
	|     |     |
	|           +- (...) Placemarks and polygons for second hole
	|        ...
	+- Golf Course No 2 Name
	|
	|     +- Facilities
	|     |    
	+- Hole 1
	...
	|
	+- Another Country Club Name
	|
	...
	
	VERSION
	$Revision$
	
	
	HISTORY
	Sep 2009: Created.
	
-->
	
	<xsl:output method="xml" version="1.0" indent="yes"/>
	
	<xsl:template match="/kml:kml/kml:Document">
		<xsl:element name="golfml">
			<xsl:attribute name="xmlns:xsi">http://www.w3.org/2001/XMLSchema-instance</xsl:attribute>
			<xsl:attribute name="xsi:schemaLocation">http://code.google.com/p/golfml ../schemas/golfml.xsd</xsl:attribute>
			<xsl:attribute name="version">0.9</xsl:attribute>
			
			<xsl:apply-templates select="kml:Folder" mode="country-club"/>			
		</xsl:element>
	</xsl:template>
	
	
	
	<xsl:template match="kml:Folder" mode="country-club">
		<xsl:element name="country-club">
			<xsl:element name="name"><xsl:value-of select="kml:name"/></xsl:element>
			<xsl:element name="address">
				<xsl:element name="country">
					<xsl:attribute name="code">US</xsl:attribute>
				</xsl:element>
				<xsl:element name="postal-code">0</xsl:element>
			</xsl:element>
			<xsl:apply-templates select="kml:Folder" mode="golf-course"/>
		</xsl:element><!--country-club-->
	</xsl:template>
	
	
	
	<xsl:template match="kml:Folder" mode="golf-course">
		<xsl:element name="golf-course">
			<xsl:element name="name"><xsl:value-of select="kml:name"/></xsl:element>
			<xsl:element name="holes">
				<xsl:apply-templates select="kml:Folder" mode="hole"/>
			</xsl:element><!--holes-->
		</xsl:element><!--golf-course-->
	</xsl:template>
	
	
	
	<xsl:template match="kml:Folder" mode="hole">
		<xsl:choose>
			<xsl:when test="contains(translate(kml:name,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'), 'facilit')">
				<xsl:element name="facilities">
					<xsl:element name="placemarks">
						<xsl:apply-templates select="kml:Placemark"/>
					</xsl:element>
				</xsl:element>
			</xsl:when>
			<xsl:otherwise>
				<xsl:element name="hole">
					<xsl:attribute name="number"><xsl:value-of select="substring-after(kml:name, ' ')"/></xsl:attribute>
					<xsl:attribute name="name"><xsl:value-of select="kml:name"/></xsl:attribute>
					<xsl:element name="placemarks">
						<xsl:apply-templates select="kml:Placemark"/>
					</xsl:element>
				</xsl:element>					
			</xsl:otherwise>
		</xsl:choose>
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
						<xsl:with-param name="coordinate" select="kml:Point/kml:coordinates"/>
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
		<xsl:element name="gps">
			<xsl:attribute name="lon"><xsl:value-of select="substring-before($coordinate, $separator)"/></xsl:attribute>
			<xsl:attribute name="lat"><xsl:value-of select="substring-before(substring-after($coordinate, $separator), $separator)"/></xsl:attribute>
			<xsl:attribute name="alt"><xsl:value-of select="substring-after(substring-after($coordinate, $separator), $separator)"/></xsl:attribute>
		</xsl:element>
	</xsl:template>
	

	
	<xsl:template name="TypeFinder">
		<xsl:param name="name"/>
		<!-- reminder: exists choose selector as soon as condition is met.
			 we choose loose names first, then try to refine.
		  -->

		<xsl:choose>
			<xsl:when test="contains($name, 'trees')">trees</xsl:when>
			<xsl:when test="contains($name, 'tree')">tree</xsl:when>
			<xsl:when test="contains($name, 'rough')">
				<xsl:choose>
					<xsl:when test="contains($name, 'semi')">semi-rough</xsl:when>
					<xsl:when test="contains($name, 'light')">semi-rough</xsl:when>
					<xsl:when test="contains($name, 'heavy')">heavy-rough</xsl:when>
					<xsl:otherwise>rough</xsl:otherwise>
				</xsl:choose>
			</xsl:when>
			<xsl:when test="contains($name, 'water')">
				<xsl:choose>
					<xsl:when test="contains($name, 'lateral')">lateral-water</xsl:when>
					<xsl:when test="contains($name, 'front')">front-water</xsl:when>
					<xsl:otherwise>water</xsl:otherwise>
				</xsl:choose>
			</xsl:when>
			<xsl:when test="contains($name, 'trap')">
				<xsl:choose>
					<xsl:when test="contains($name, 'fairway')">fairway-trap</xsl:when>
					<xsl:when test="contains($name, 'green')">greenside-trap</xsl:when>
					<xsl:when test="contains($name, 'grass')">grass-trap</xsl:when>
					<xsl:when test="contains($name, 'sand')">sand-trap</xsl:when>
					<xsl:otherwise>trap</xsl:otherwise>
				</xsl:choose>
			</xsl:when>
			<xsl:when test="contains($name, 'bunker')">
				<xsl:choose>
					<xsl:when test="contains($name, 'fairway')">fairway-trap</xsl:when>
					<xsl:when test="contains($name, 'green')">greenside-trap</xsl:when>
					<xsl:when test="contains($name, 'grass')">grass-trap</xsl:when>
					<xsl:when test="contains($name, 'sand')">sand-trap</xsl:when>
					<xsl:otherwise>trap</xsl:otherwise>
				</xsl:choose>
			</xsl:when>
			<xsl:when test="contains($name, 'green')">
				<xsl:choose>
					<xsl:when test="contains($name, 'trap')">greenside-trap</xsl:when>
					<xsl:when test="contains($name, 'bunker')">greenside-trap</xsl:when>
					<xsl:when test="contains($name, 'side')">greenside-trap</xsl:when>
					<xsl:otherwise>green</xsl:otherwise>
				</xsl:choose>
			</xsl:when>
			<xsl:when test="contains($name, 'obstruction')">
				<xsl:choose>
					<xsl:when test="contains($name, 'movable')">obstruction</xsl:when>
					<xsl:when test="contains($name, 'immovable')">immovable-obstruction</xsl:when>
					<xsl:otherwise>obstruction</xsl:otherwise>
				</xsl:choose>
			</xsl:when>
			<xsl:when test="contains($name, 'marker')">
				<xsl:choose>
					<xsl:when test="contains($name, '100')">marker-100</xsl:when>
					<xsl:when test="contains($name, '135')">marker-135</xsl:when>
					<xsl:when test="contains($name, '150')">marker-150</xsl:when>
					<xsl:when test="contains($name, '200')">marker-200</xsl:when>
					<xsl:otherwise>marker</xsl:otherwise>
				</xsl:choose>
			</xsl:when>
			<!-- very specific words -->
			<xsl:when test="contains($name, 'contour')">hole-contour</xsl:when>
			<xsl:when test="contains($name, 'tee')">tee</xsl:when>
			<xsl:when test="contains($name, 'fairway')">fairway</xsl:when>
			<xsl:when test="contains($name, 'bush')">bush</xsl:when>
			<xsl:when test="contains($name, 'fringe')">fringe</xsl:when>
			<xsl:when test="contains($name, 'path')">path</xsl:when>
			<xsl:when test="contains($name, 'building')">building</xsl:when>
			<xsl:when test="contains($name, 'construction')">building</xsl:when>
			<xsl:when test="contains($name, 'out of bound')">out-of-bound</xsl:when>
			<xsl:when test="contains($name, 'oob')">out-of-bound</xsl:when>
			<xsl:when test="contains($name, 'dogleg')">dogleg</xsl:when>
			<xsl:when test="contains($name, 'sprinkler')">sprinkler</xsl:when>
			<xsl:when test="contains($name, 'aim')">aim</xsl:when>
			
			<xsl:otherwise>other</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	
</xsl:stylesheet>
