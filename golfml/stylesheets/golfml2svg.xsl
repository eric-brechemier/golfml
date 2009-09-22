<?xml version="1.0" encoding="ISO-8859-1"?>
<?xml-stylesheet href="../stylesheets/golfml.css" type="text/css"?>
<xsl:stylesheet
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	version="1.0"
	xmlns="http://www.w3.org/2000/svg"
	xmlns:g="http://code.google.com/p/golfml"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xmlns:xlink="http://www.w3.org/1999/xlink"
	xmlns:math="http://exslt.org/math"
	>
	<!-- golfml2svg.xsl
    
	DESCRIPTION
    
		GolfML hole description to SVG graphics.
		Works either for a single hole or for an entire golf course (1 to 18 holes).
     
      
     VERSION
     	$Revision$
     	
     HISTORY
     	Sep 2009: Created.
     	
	-->
	
	<xsl:output method="xml" version="1.0" indent="yes"/>
	
	<xsl:param name="width">600</xsl:param>
	<xsl:param name="height"><xsl:value-of select="$width"/></xsl:param> <!-- canvas is square for now... -->
	
	<xsl:template match="/">
		<xsl:apply-templates select="g:golfml/g:country-club/g:golf-course" />
	</xsl:template>

	<xsl:template match="g:golf-course">
		<xsl:element name="svg">
			<xsl:attribute name="width"><xsl:value-of select="$width"/></xsl:attribute>
			<xsl:attribute name="height"><xsl:value-of select="$height"/></xsl:attribute>
			<xsl:attribute name="xmlns:xlink">http://www.w3.org/1999/xlink</xsl:attribute>
			
			
			<!--
			<xsl:element name="defs">
				<xsl:element name="g">
					<xsl:attribute name="id">Dogleg</xsl:attribute>
					<xsl:attribute name="class">aim</xsl:attribute>
					<xsl:element name="circle">
						<xsl:attribute name="r">5</xsl:attribute>
						<xsl:attribute name="cx">-2</xsl:attribute>
						<xsl:attribute name="cy">-2</xsl:attribute>
					</xsl:element>
				</xsl:element>
			</xsl:element>
			-->
			<xsl:call-template name="Defs"/>
			
			<xsl:variable name="min_lat">
				<xsl:for-each select=".//g:lat">
					<xsl:sort data-type="number" />
					<xsl:if test="position() = 1">
						<xsl:value-of select="number(.)" />
					</xsl:if>
				</xsl:for-each>
			</xsl:variable>
			<xsl:variable name="max_lat">
				<xsl:for-each select=".//g:lat">
					<xsl:sort data-type="number" order="descending"/>
					<xsl:if test="position() = 1">
						<xsl:value-of select="number(.)" />
					</xsl:if>
				</xsl:for-each>
			</xsl:variable>
			<xsl:variable name="min_lon">
				<xsl:for-each select=".//g:lon">
					<xsl:sort data-type="number" />
					<xsl:if test="position() = 1">
						<xsl:value-of select="number(.)" />
					</xsl:if>
				</xsl:for-each>
			</xsl:variable>
			<xsl:variable name="max_lon">
				<xsl:for-each select=".//g:lon">
					<xsl:sort data-type="number" order="descending"/>
					<xsl:if test="position() = 1">
						<xsl:value-of select="number(.)" />
					</xsl:if>
				</xsl:for-each>
			</xsl:variable>

			<xsl:variable name="scale">
				<xsl:if test="(number($max_lat)-number($min_lat)) >= (number($max_lon)-number($min_lon))">
					<xsl:value-of select="$width div (number($max_lat)-number($min_lat))"/>
				</xsl:if>
				<xsl:if test="(number($max_lat)-number($min_lat)) &lt; (number($max_lon)-number($min_lon))">
					<xsl:value-of select="$width div (number($max_lon)-number($min_lon))"/>
				</xsl:if>
			</xsl:variable>
												
			<!-- TO DO: Apply placemark in layout order: large objects (hole-contour, etc.) first and
				 smaller objects (tee, greens, poi) last.
				 
				 01 = hole-contour
				 02 = out-of-bound
				 03 = heavy-rough
				 04 = rough
				 05 = semi-rough
				 06 = bush
				 
				 07 = lateral-water
				 08 = front-water
				 09 = water
				 
				 10 = tee
				 11 = fairway
				 12 = path
				 13 = other
				 14 = building
				 15 = obstruction
				 
				 16 = trap
				 17 = greenside-trap
				 18 = fairway-trap
				 19 = bunker
				 20 = fringe
				 21 = green
				 
				 22 = trees
				 23 = tree
				 
				 24 = aim
				 25 = marker
				 
			-->
			<xsl:element name="clipPath">
				<xsl:attribute name="id">ClipPath</xsl:attribute>
				<xsl:element name="rect">
					<xsl:attribute name="x">0</xsl:attribute>
					<xsl:attribute name="y">0</xsl:attribute>
					<xsl:attribute name="width"><xsl:value-of select="$width"/></xsl:attribute>
					<xsl:attribute name="height"><xsl:value-of select="$height"/></xsl:attribute>
				</xsl:element>
			</xsl:element>
			<xsl:element name="g">
				<xsl:attribute name="clip-path">url(#ClipPath)</xsl:attribute>

				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='hole-contour']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='out-of-bound']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='heavy-rough']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='rough']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='semi-rough']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='bush']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='				 
					lateral-water']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='front-water']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='water']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='tee']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='fairway']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='path']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='other']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='building']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='obstruction']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='trap']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='greenside-trap']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='fairway-trap']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='bunker']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='fringe']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='green']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='trees']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='tree']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='aim']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:aoi[@type='marker']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='hole-contour']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='out-of-bound']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='heavy-rough']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='rough']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='semi-rough']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='bush']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='				 
					lateral-water']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='front-water']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='water']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='tee']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='fairway']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='path']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='other']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='building']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='obstruction']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='trap']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='greenside-trap']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='fairway-trap']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='bunker']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='fringe']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='green']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='trees']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='tree']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='aim']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
				<xsl:apply-templates select=".//g:placemarks/g:poi[@type='marker']">
					<xsl:with-param name="minlat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="minlon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
				</xsl:apply-templates>
			</xsl:element> <!-- g clip-path -->
			
		</xsl:element><!-- svg -->
	</xsl:template>
	
	
	<xsl:template match="g:placemarks">
		<xsl:param name="minlat"/>
		<xsl:param name="minlon"/>
		<xsl:param name="scale"/>
		<xsl:apply-templates>
			<xsl:with-param name="minlat"><xsl:value-of select="$minlat"/></xsl:with-param>
			<xsl:with-param name="minlon"><xsl:value-of select="$minlon"/></xsl:with-param>
			<xsl:with-param name="scale"><xsl:value-of select="$scale"/></xsl:with-param>
		</xsl:apply-templates>
	</xsl:template>


	<xsl:template match="g:poi">
		<xsl:param name="minlat"/>
		<xsl:param name="minlon"/>
		<xsl:param name="scale"/>
		<xsl:element name="use">
			<xsl:attribute name="x"><xsl:value-of select="(number(g:position/g:position-gps/g:lat)-number($minlat))*$scale"/></xsl:attribute>
			<xsl:attribute name="y"><xsl:value-of select="(number(g:position/g:position-gps/g:lon)-number($minlon))*$scale"/></xsl:attribute>
			<xsl:attribute name="xlink:href">#Dogleg</xsl:attribute>
		</xsl:element>
	</xsl:template>


	<xsl:template match="g:aoi">
		<xsl:param name="minlat"/>
		<xsl:param name="minlon"/>
		<xsl:param name="scale"/>
		<xsl:element name="path">
			<xsl:attribute name="class"><xsl:value-of select="@type"/></xsl:attribute>
			<xsl:attribute name="class"><xsl:value-of select="@type"/></xsl:attribute>
			<xsl:attribute name="d"><xsl:text>M </xsl:text>
				<xsl:for-each select="g:position">
					<xsl:sort select="@number" data-type="number"/>
					<xsl:if test="position() = 2"><xsl:text>L </xsl:text></xsl:if>
					<xsl:apply-templates select="g:position-gps">
						<xsl:with-param name="minlat"><xsl:value-of select="$minlat"/></xsl:with-param>
						<xsl:with-param name="minlon"><xsl:value-of select="$minlon"/></xsl:with-param>
						<xsl:with-param name="scale"><xsl:value-of select="$scale"/></xsl:with-param>
					</xsl:apply-templates>
				</xsl:for-each><xsl:text>Z</xsl:text>
			</xsl:attribute>
		</xsl:element>		
	</xsl:template>
	
	
	<xsl:template match="g:position-gps">
		<xsl:param name="minlat"/>
		<xsl:param name="minlon"/>
		<xsl:param name="scale"/>
		<xsl:value-of select="(number(g:lat)-number($minlat))*$scale"/>,<xsl:value-of select="(number(g:lon)-number($minlon))*$scale"/><xsl:text> </xsl:text>
	</xsl:template>
	
	
	<xsl:template name="Defs">
			<defs>
		<style type="text/css"><![CDATA[
/* Stylesheet for SVG elements
 *
 *
 * Classes for area of interest (surfaces)
 *
 * DRAWING
 */
.background {
		fill: url(#background-gradient);
}

.hole-limits {
	fill: none;
	stroke: #ccc;
	stroke-width: 1px;
	stroke-dasharray: 10,10;
}

.hole-contour {
	fill: #030;
	stroke: #fff;
	stroke-width: 2px;
}


/* GROUND
 */
.fairway {
	fill: url(#fairway-pattern);
	stroke: #0b0;
	stroke-width: 3px; /* fairway fringe... */
}

.tee {
	fill: #00c800;
	stroke: #0f0;
	stroke-width: 1px;
}

.fringe {
	fill: #0A0;
}

.green {
	fill: url(#green-gradient);
	stroke: #0f0;
	stroke-width: 1px;
}

.semi-rough,
.light-rough {
	fill: #080;
	stroke: #08800D;
	stroke-width: 1px;
}

.rough {
	fill: url(#rough-gradient);
	stroke: #07660A;
	stroke-width: 1px;
}

.heavy-rough {
	fill: #040;
	stroke: #074D08;
	stroke-width: 1px;
}

.ground {
	fill: #7f400c;
	stroke: #fdcc6a;
	stroke-width: 1px;
}


/* WATER
 */
.lateral-water {
	fill: url(#lateral-water-gradient);
	stroke: red;
	stroke-width: 4px;
}

.front-water, .water {
	fill: #37f;
	stroke: yellow;
	stroke-width: 3px;
}


/* SPECIAL
 */
.under-repair {
	fill: none;
	stroke: #00d;
	stroke-width: 2px;
}

.out-of-bound { /* line only */
	fill: none;
	stroke: white;
	stroke-width: 4px;
}

.out-of-bound-area {
	fill: #ccc;
	stroke: white;
	stroke-width: 4px;
}


/* TRAPS
 */
.trap,
.bunker {
	fill: #92cf94;
	stroke: #4c994c;
	stroke-width: 1px;
}

.grass-trap,
.grass-bunker {
	fill: #050;
	stroke: #0A0;
	stroke-width: 1px;
}

.sand-trap,
.sand-bunker,
.greenside-trap,
.greenside-bunker {
	fill: #eec;
	stroke: #040;
	stroke-width: 2px;
}

.fairway-trap,
.fairway-bunker {
	fill: #eec;
	stroke: #030;
	stroke-width: 2px;
}


/* OBSTACLES
 */
.tree {
	fill: #062;
	stroke: #0c6;
	stroke-width: 1px;
}

.trees {
	fill: #062;
	stroke: #0c6;
	stroke-width: 1px;
}

.bush {
	fill: #00b259;
	stroke: #7f400c;
	stroke-width: 1px;
}


.path {
	fill: #666;
	stroke: #bbb;
	stroke-width: 1px;
}

.building,
.construction {
	fill: #fdcc6a;
	stroke: #7f400C;
	stroke-width: 1px;
}

.obstruction {
	fill: #555;
	stroke: #999;
	stroke-width: 1px;
}


.other {
	fill: #ddd;
	stroke: #666;
	stroke-width: 1px;
}

/* Classes for point of interest (points)
 *
 */
.dogleg {
	fill: #000;
}

.aim {
	fill: #fff;
	stroke: #f00;
	stroke-width: 1px;
}

.tees {
	fill: #00f;
}

.tees-hollow {
	stroke: #fff;
	stroke-width: 0.5px;
}

.hole {
	fill: #fff;
}

.distance-marker-close {
	fill: #44f;
	stroke: #00f;
	stroke-width: 1px;
}

.distance-marker,
.distance-marker-mid {
	fill: #f44;
	stroke: #f00;
	stroke-width: 1px;
}

.distance-marker-far {
	fill: #dd4;
	stroke: #ff0;
	stroke-width: 1px;
}

.green-region-delimiter {
	fill: none;
	stroke: #fff;
	stroke-width: 0.5;
	stroke-dasharray: 4,2;
}

/* Classes for special decorative objects
 *
 */
.compass {
	fill: #ddd;
	stroke: #fff;
	stroke-width: 1px;
}

     ]]></style>
		
		<linearGradient id="background-straight">
			<stop offset="5%" stop-color="#002"/>
			<stop offset="95%" stop-color="#004"/>
		</linearGradient>		
		<linearGradient id="background-gradient" gradientTransform="rotate(30)" xlink:href="#background-straight"/>
		
		<linearGradient id="lateral-water-straight">
			<stop offset="5%" stop-color="#006"/>
			<stop offset="95%" stop-color="#008"/>
		</linearGradient>		
		<linearGradient id="lateral-water-gradient" gradientTransform="rotate(60)" xlink:href="#lateral-water-straight"/>
		
		<linearGradient id="rough-straight">
			<stop offset="0%" stop-color="#030"/>
			<stop offset="25%" stop-color="#060"/>
			<stop offset="50%" stop-color="#040"/>
			<stop offset="75%" stop-color="#060"/>
			<stop offset="100%" stop-color="#080"/>
		</linearGradient>		
		<linearGradient id="rough-gradient" gradientTransform="rotate(70)" xlink:href="#rough-straight"/>
		
		<linearGradient id="green-straight">
			<stop offset="0%" stop-color="#0d0"/>
			<stop offset="25%" stop-color="#0e0"/>
			<stop offset="50%" stop-color="#0c0"/>
			<stop offset="75%" stop-color="#0e0"/>
			<stop offset="100%" stop-color="#0b0"/>
		</linearGradient>		
		<linearGradient id="green-gradient" gradientTransform="rotate(75)" xlink:href="#green-straight"/>
		
		<pattern id="fairway-straight" patternUnits="userSpaceOnUse" x="0" y="0" width="20" height="20" viewBox="0 0 20 20">	
			<rect x="0" y="0" width="10" height="10" fill="#00c000"/>
			<rect x="10" y="10" width="10" height="10" fill="#00c800"/>
			<rect x="10" y="0" width="10" height="10" fill="#00bc00"/>
			<rect x="0" y="10" width="10" height="10" fill="#00b800"/>
		</pattern>
		<pattern id="fairway-pattern" patternTransform="rotate(60)" xlink:href="#fairway-straight"/>
		
		<g id="TeeSet">
			<circle cx="-5" cy="0" r="2"/>
			<circle cx="5" cy="0" r="2"/>			
		</g>
		
		<g id="DistanceMarker">
			<circle r="6"/>
		</g>
		
		<g id="Dogleg">
			<circle r="4"/>
		</g>
		
		<g id="Hole">
			<circle r="1.2"/>
		</g>
		
		<g id="Compass">
			<polygon id="Needle"        class="compass-needle" points="0,30 -8,0 0,-20 8,0"/>
			<circle  id="CompassCenter" class="compass"        r="10"/>
			<text id="North" x="-4.7" y="40">N</text>
		</g>
		
	</defs>

	</xsl:template>

</xsl:stylesheet>
