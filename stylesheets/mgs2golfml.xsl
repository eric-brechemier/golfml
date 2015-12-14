<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0"
				xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
				xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
				xmlns:xlink="http://www.w3.org/1999/xlink"
				xmlns:g="http://code.google.com/p/golfml"
	>
<!-- mgs2golfml.xsl

DESCRIPTION

Mobile Golf Scorer to GolfML transformation style sheet
 
MGS provides 2 different XML outputs:

	1. Golf course definition
       2. Rounds of golf
   
A Round of golf contains the definition of the course played.
  
 VERSION
     $Revision$
     
 HISTORY
     Aug 2009: Created with MGS Version 1.40.
     
-->
	<xsl:output method="xml" indent="yes"/>
	
	<xsl:template match="/">
		<xsl:element name="golfml">
			<xsl:attribute name="xmlns">http://code.google.com/p/golfml</xsl:attribute>
			<xsl:attribute name="xsi:schemaLocation"><xsl:text>http://code.google.com/p/golfml ../../schemas/golfml.xsd</xsl:text></xsl:attribute>
			<xsl:attribute name="xmlns:xsi">http://www.w3.org/2001/XMLSchema-instance</xsl:attribute>
			<xsl:attribute name="version">0.9</xsl:attribute>

			<xsl:apply-templates select="GOLFCOURSE"/>

			<xsl:apply-templates select="GOLFCOURSE/GOLFROUND">
				<xsl:with-param name="country-club-name" select="GOLFCOURSE/NAME"/>
				<xsl:with-param name="country-club-city" select="GOLFCOURSE/CITY"/>
			</xsl:apply-templates>

		</xsl:element>
	</xsl:template>

	<xsl:template match="GOLFCOURSE">
		<xsl:element name="country-club">
			<xsl:element name="name">
				<xsl:value-of select="NAME"/>
			</xsl:element>
			<xsl:element name="address">
				<xsl:element name="street">
					<xsl:value-of select="ADRESS"/>
				</xsl:element>
				<xsl:element name="postal-code">
					<xsl:value-of select="CITY"/>
				</xsl:element>
				<xsl:element name="country">
					<xsl:attribute name="code">XX</xsl:attribute>
					<xsl:value-of select="COUNTRY"/>
				</xsl:element>
			</xsl:element>
			<xsl:element name="contact">
				<xsl:attribute name="type">club-house</xsl:attribute>
				<xsl:element name="phone">
					<xsl:value-of select="translate(PHONE,'() ','--')"/>
				</xsl:element>
			</xsl:element>

			<xsl:element name="golf-course">
				<xsl:element name="name">
					<xsl:value-of select="NAME"/>
				</xsl:element>
				<xsl:apply-templates select="GOLFCOURSETEES/GOLFCOURSETEE"/>
			</xsl:element>
			
		</xsl:element>
	</xsl:template>

	<xsl:template match="GOLFCOURSETEE">
		<xsl:element name="tee-set">
			<xsl:attribute name="name">
				<xsl:value-of select="TEENAME"/>
			</xsl:attribute>
			<xsl:element name="qualification">
				<xsl:element name="qualification-usga">
					<xsl:element name="rating">
						<xsl:value-of select="TEECR"/>
					</xsl:element>
					<xsl:element name="slope">
						<xsl:value-of select="TEESLOPE"/>
					</xsl:element>
				</xsl:element>
			</xsl:element>
			<xsl:apply-templates select="TEEHOLES/TEEHOLE"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="TEEHOLE">
		<xsl:element name="tee">
			<xsl:attribute name="number">
				<xsl:value-of select="TEEHOLENUM"/>
			</xsl:attribute>
			<xsl:element name="par">
				<xsl:value-of select="TEEHOLEPAR"/>
			</xsl:element>
			<xsl:element name="handicap-stroke">
				<xsl:value-of select="TEEHOLEHCP"/>
			</xsl:element>
			<xsl:element name="length">
				<xsl:attribute name="units">meters</xsl:attribute>
				<xsl:value-of select="TEEHOLELEN/METER"/>
			</xsl:element>
			<xsl:element name="length">
				<xsl:attribute name="units">yards</xsl:attribute>
				<xsl:value-of select="TEEHOLELEN/YARDS"/>
			</xsl:element>
		</xsl:element>
	</xsl:template>


	<xsl:template match="GOLFROUND">
		<xsl:param name="country-club-name"/>
		<xsl:param name="country-club-city"/>
		
		<xsl:apply-templates select="GOLFROUNDPLAYER">
			<xsl:with-param name="country-club-name" select="$country-club-name"/>
			<xsl:with-param name="country-club-city" select="$country-club-city"/>
			<xsl:with-param name="golf-course-name" select="GOLFROUNDCOURSE"/>
			<xsl:with-param name="round-date" select="GOLFROUNDDATEYYYYMMDD"/>
		</xsl:apply-templates>	
	</xsl:template>
	
	
	<xsl:template match="GOLFROUNDPLAYER">
		<xsl:param name="country-club-name"/>
		<xsl:param name="country-club-city"/>
		<xsl:param name="golf-course-name"/>
		<xsl:param name="round-date"/>
		
		<xsl:element name="player">
			<xsl:attribute name="gender">
				<xsl:if test="GENDER = 'Male'">gentlemen</xsl:if>
				<xsl:if test="GENDER = 'Female'">ladies</xsl:if>
			</xsl:attribute>
			<xsl:element name="name">
				<xsl:if test="NAME"><xsl:value-of select="NAME"/></xsl:if>
				<xsl:if test="LASTNAME">
					<xsl:value-of select="FIRSTNAME"/>
					<xsl:value-of select="LASTNAME"/>
				</xsl:if>
			</xsl:element>
			<xsl:element name="contact">
				<xsl:attribute name="type">home</xsl:attribute>
				<xsl:element name="phone">
					<xsl:value-of select="translate(PHONE,'() ','--')"/>
				</xsl:element>
				<xsl:element name="email">
					<xsl:value-of select="EMAIL"/>
				</xsl:element>
			</xsl:element>
			<xsl:element name="home-country-club">
				<xsl:value-of select="CLUBATTACHED"/>
			</xsl:element>	
			<xsl:element name="date-of-birth">2000-01-01</xsl:element> <!-- mandatory in golfml, not supplied by MGS -->

			<xsl:element name="round">
				<xsl:element name="date">
					<xsl:value-of select="concat($round-date,'T00:00:00')"/>
				</xsl:element>
				<xsl:element name="scorecard">
					<xsl:element name="tees">
						<xsl:element name="country-club.name">
							<xsl:value-of select="$country-club-name"/>
						</xsl:element>
						<!-- mandatory in golfml, not supplied by MGS -->
						<xsl:element name="country-club.address.country.iso3166">
							<xsl:value-of select="string('XX')"/>
						</xsl:element>
						<xsl:element name="country-club.address.postal-code">
							<xsl:value-of select="$country-club-city"/>
						</xsl:element>
						<xsl:element name="country-club.golf-course.name">
							<xsl:value-of select="$golf-course-name"/>
						</xsl:element>
						<xsl:element name="country-club.golf-course.tee-set.name">
							<xsl:value-of select="TEE"/>
						</xsl:element>
					</xsl:element>
					<xsl:element name="handicap-strokes">
						<xsl:value-of select="HCP"/>
					</xsl:element>

					<xsl:apply-templates select="GOLFROUNDRESULTS"/>

				</xsl:element>

			</xsl:element>
		</xsl:element>
	</xsl:template>

	<xsl:template match="GOLFROUNDRESULTS">
		<xsl:element name="score">
			<xsl:apply-templates select="GOLFROUNDRESULT"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="GOLFROUNDRESULT">
		<xsl:element name="hole">
			<xsl:attribute name="number">
				<xsl:value-of select="ROUNDHOLENUM"/>
			</xsl:attribute>
			<xsl:element name="strokes">
				<xsl:value-of select="ROUNDHOLESTROKES"/>
			</xsl:element>
			<xsl:element name="putts">
				<xsl:value-of select="ROUNDHOLEPUTTS"/>
			</xsl:element>
			<xsl:element name="points">
				<xsl:value-of select="ROUNDHOLEPOINTS"/>
			</xsl:element>
			<xsl:element name="statistics">
				<xsl:element name="fairway">
					<xsl:value-of select="ROUNDHOLEFWH"/>
				</xsl:element>
				<xsl:element name="green-in-regulation">
					<xsl:value-of select="ROUNDHOLEGIR"/>
				</xsl:element>
			</xsl:element>
			<xsl:apply-templates select="ROUNDHOLESTROKEDETAILS"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="ROUNDHOLESTROKEDETAILS">
		<xsl:apply-templates select="ROUNDHOLESTROKE"/>
	</xsl:template>

	<xsl:template match="ROUNDHOLESTROKE">
		<xsl:element name="stroke">
			<xsl:attribute name="number">
				<xsl:value-of select="ROUNDHOLESTROKENUM"/>
			</xsl:attribute>
			<xsl:element name="golf-club">
				<xsl:value-of select="ROUNDHOLESTROKECLUB"/>
			</xsl:element>
			<xsl:element name="landing">
				<!-- Translation from MGS to GolfML vocabulary:
					
						Landing areas:
							Fairway
							Fairway Bunker
							Semi-Rough
							Rough
							On the Fringe
							On the Green
							Green Bunker
							In the Hole
							Front Water
							Lateral Water
							Out of Bounds
				-->
				<xsl:choose>
					<xsl:when test="ROUNDHOLESTROKELANDING = 'Fairway'">fairway</xsl:when>
					<xsl:when test="ROUNDHOLESTROKELANDING = 'Fairway Bunker'">fairway-trap</xsl:when>
					<xsl:when test="ROUNDHOLESTROKELANDING = 'Semi-Rough'">semi-rough</xsl:when>
					<xsl:when test="ROUNDHOLESTROKELANDING = 'Rough'">fairway</xsl:when>
					<xsl:when test="ROUNDHOLESTROKELANDING = 'On the Fringe'">fringe</xsl:when>
					<xsl:when test="ROUNDHOLESTROKELANDING = 'On the Green'">green</xsl:when>
					<xsl:when test="ROUNDHOLESTROKELANDING = 'Green Bunker'">green-trap</xsl:when>
					<xsl:when test="ROUNDHOLESTROKELANDING = 'In the Hole'">hole</xsl:when>
					<xsl:when test="ROUNDHOLESTROKELANDING = 'Front Water'">front-water</xsl:when>
					<xsl:when test="ROUNDHOLESTROKELANDING = 'Lateral Water'">lateral-water</xsl:when>
					<xsl:when test="ROUNDHOLESTROKELANDING = 'Out of Bounds'">out-of-bound</xsl:when>
					<xsl:otherwise>other</xsl:otherwise>
				</xsl:choose>
			</xsl:element>
			<xsl:if test="ROUNDHOLESTROKELENGTHMETERS">
				<xsl:element name="distance">
					<xsl:attribute name="units">meters</xsl:attribute>
					<xsl:value-of select="ROUNDHOLESTROKELENGTHMETERS"/>
				</xsl:element>
			</xsl:if>
			<xsl:element name="precision">
				<!-- Translation from MGS to GolfML vocabulary:
					
						Left/Right:
						 0=Center
						-2=Far Left
						 2=Far Right
						-1=Left
						 1=Right
						 
						Front/Back:
						 1=Long
						 0=Normal
						-1=Short
						 2=Very Long
						-2=Very Short
				-->
				<xsl:element name="front-back">
					<xsl:choose>
						<xsl:when test="ROUNDHOLESTROKELENGTH = 'Very Short'">-2</xsl:when>
						<xsl:when test="ROUNDHOLESTROKELENGTH = 'Short'">-1</xsl:when>
						<xsl:when test="ROUNDHOLESTROKELENGTH = 'Normal'">0</xsl:when>
						<xsl:when test="ROUNDHOLESTROKELENGTH = 'Long'">1</xsl:when>
						<xsl:when test="ROUNDHOLESTROKELENGTH = 'Very Long'">2</xsl:when>
					</xsl:choose>
				</xsl:element>
				<xsl:element name="left-right">
					<xsl:choose>
						<xsl:when test="ROUNDHOLESTROKEDIRECTION = 'Far Left'">-2</xsl:when>
						<xsl:when test="ROUNDHOLESTROKEDIRECTION = 'Left'">-1</xsl:when>
						<xsl:when test="ROUNDHOLESTROKEDIRECTION = 'Center'">0</xsl:when>
						<xsl:when test="ROUNDHOLESTROKEDIRECTION = 'Right'">1</xsl:when>
						<xsl:when test="ROUNDHOLESTROKEDIRECTION = 'Far Right'">2</xsl:when>
					</xsl:choose>
				</xsl:element>
			</xsl:element>
			<xsl:if test="ROUNDHOLESTROKEPUTTLENGTH">
				<xsl:element name="distance-text">
					<xsl:value-of select="ROUNDHOLESTROKEPUTTLENGTH"/>
				</xsl:element>
			</xsl:if>

		</xsl:element>
	</xsl:template>

</xsl:stylesheet>
