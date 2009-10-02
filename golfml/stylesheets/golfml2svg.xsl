<?xml version="1.0" encoding="ISO-8859-1"?>
<?xml-stylesheet href="../stylesheets/golfml.css" type="text/css"?>
<xsl:stylesheet version="2.0"
				xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
				xmlns:xs="http://www.w3.org/2001/XMLSchema"
				xmlns="http://www.w3.org/2000/svg"
				xmlns:g="http://code.google.com/p/golfml"
				xmlns:xlink="http://www.w3.org/1999/xlink"
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
	
	<!-- File format for multiple file output -->
	<xsl:output method="xml" version="1.0" indent="yes" name="SVG"/>
	<xsl:output method="text" version="1.0" indent="yes" name="Text"/>
	<!-- size of individual outputs in pixels
		 Please note that Defs are scaled for drawing between roughly 400 and 1200 pixels.
		 Smaller or larger values for drawing would need scaling of Defs (patterns, stokes, elements...)
	  -->
	<xsl:param name="width">600</xsl:param>
	<xsl:param name="height">600</xsl:param>
	<!-- position of compass, relative to upper left corner, in pixels -->
	<xsl:param name="compassx"><xsl:value-of select="$width - 50"/></xsl:param>
	<xsl:param name="compassy"><xsl:value-of select="50"/></xsl:param>
	<!-- mode=course|hole, generate a single SVG file for the entire course, or one for each hole -->
	<xsl:param name="mode">hole</xsl:param>
	<!-- If a hole-number is supplied, generates only that hole -->
	<xsl:param name="hole-number">15</xsl:param>
	<!-- TO DO: Apply placemark in layout order: large objects (hole-contour, etc.) first and
		== smaller objects (tee, greens, poi) last. Here is suggested list of ordered values.
		== 
		== 01 = hole-contour
		== 02 = out-of-bound
		== 03 = heavy-rough
		== 04 = rough
		== 05 = semi-rough
		== 06 = bush
		== 
		== 07 = lateral-water
		== 08 = front-water
		== 09 = water
		== 
		== 10 = tee
		== 11 = fairway
		== 12 = path
		== 13 = other
		== 14 = building
		== 15 = obstruction
		== 
		== 16 = trap
		== 17 = greenside-trap
		== 18 = fairway-trap
		== 19 = bunker
		== 20 = fringe
		== 21 = green
		== 
		== 22 = trees
		== 23 = tree
		== 
		== 24 = aim
		== 25 = marker			 
		
		The typelist parameter is a list of area/point of interest types in the sequence order it should be drawn.
		Area are always drawn first, and points are drawn on top.
	-->
	
	<xsl:param name="typelist">hole-contour out-of-bound heavy-rough rough semi-rough bush lateral-water front-water water tee fairway path other building obstruction trap greenside-trap fairway-trap bunker fringe green trees tree aim marker</xsl:param>

	<xsl:template match="/g:golfml/g:country-club">
		<xsl:apply-templates select="g:golf-course"/>
	</xsl:template>
	

	<xsl:template match="g:golf-course">
		<xsl:apply-templates select="g:holes" mode="head"/>
	</xsl:template>


	<xsl:template match="g:holes" mode="head">
		<xsl:if test="$mode = 'course'">
			<!-- Mode 1: generate a single SVG for each hole -->
			<xsl:apply-templates select="." mode="draw"/>
		</xsl:if>
		<xsl:if test="$mode = 'hole'">
			<!-- Mode 2: generate a single SVG for each hole -->
			<xsl:choose>
				<xsl:when test="$hole-number > 0">
					<xsl:apply-templates select="g:hole[@number=$hole-number]" mode="draw"/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:for-each select="g:hole">
						<xsl:variable name="uri" select="concat('hole-', @number, '.svg')"/>
						<xsl:result-document href="{$uri}" format="SVG">
							<xsl:apply-templates select="." mode="draw"/>
						</xsl:result-document>
					</xsl:for-each>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:if>
	</xsl:template>
	

	<xsl:template match="g:hole|g:holes" mode="draw">
		<!-- draws either a single hole or a set of holes+"facilities".
		  -->
		<xsl:element name="svg">
			<xsl:attribute name="width"><xsl:value-of select="$width"/></xsl:attribute>
			<xsl:attribute name="height"><xsl:value-of select="$height"/></xsl:attribute>
			
			<xsl:call-template name="Defs"/>
			
			<xsl:element name="rect">
				<xsl:attribute name="class">background</xsl:attribute>
				<xsl:attribute name="x">0</xsl:attribute>
				<xsl:attribute name="y">0</xsl:attribute>
				<xsl:attribute name="width"><xsl:value-of select="$width"/></xsl:attribute>
				<xsl:attribute name="height"><xsl:value-of select="$height"/></xsl:attribute>
			</xsl:element>
			
			<!-- Compute bounding box for lat/lon which is mapped to the projected bounding box (width x height) -->	
			<xsl:variable name="min_lat"><xsl:value-of select="min(.//g:lat)"/></xsl:variable>
			<xsl:variable name="max_lat"><xsl:value-of select="max(.//g:lat)"/></xsl:variable>
			<xsl:variable name="min_lon"><xsl:value-of select="min(.//g:lon)"/></xsl:variable>
			<xsl:variable name="max_lon"><xsl:value-of select="max(.//g:lon)"/></xsl:variable>	
			<xsl:variable name="mid_lat"><xsl:value-of select="(number($max_lat)+number($min_lat)) div 2"/></xsl:variable>
			<xsl:variable name="mid_lon"><xsl:value-of select="(number($max_lon)+number($min_lon)) div 2"/></xsl:variable>
			
			<xsl:variable name="minmin">
				<xsl:call-template name="Project">
					<xsl:with-param name="lat"><xsl:value-of select="$min_lat"/></xsl:with-param>
					<xsl:with-param name="lon"><xsl:value-of select="$min_lon"/></xsl:with-param>
					<xsl:with-param name="midlat"><xsl:value-of select="$mid_lat"/></xsl:with-param>
					<xsl:with-param name="midlon"><xsl:value-of select="$mid_lon"/></xsl:with-param>
					<xsl:with-param name="scale"><xsl:value-of select="number(1)"/></xsl:with-param>
				</xsl:call-template>
			</xsl:variable>
			<xsl:variable name="maxmax">
				<xsl:call-template name="Project">
					<xsl:with-param name="lat"><xsl:value-of select="$max_lat"/></xsl:with-param>
					<xsl:with-param name="lon"><xsl:value-of select="$max_lon"/></xsl:with-param>
					<xsl:with-param name="midlat"><xsl:value-of select="$mid_lat"/></xsl:with-param>
					<xsl:with-param name="midlon"><xsl:value-of select="$mid_lon"/></xsl:with-param>
					<xsl:with-param name="scale"><xsl:value-of select="number(1)"/></xsl:with-param>
				</xsl:call-template>
			</xsl:variable>
			<xsl:variable name="x1"><xsl:value-of select="substring-before($minmin, ',')"/></xsl:variable>
			<xsl:variable name="y1"><xsl:value-of select="substring-after($minmin, ',')"/></xsl:variable>
			<xsl:variable name="x2"><xsl:value-of select="substring-before($maxmax, ',')"/></xsl:variable>
			<xsl:variable name="y2"><xsl:value-of select="substring-after($maxmax, ',')"/></xsl:variable>
			<xsl:variable name="minx">
				<xsl:if test="$x1 >=   $x2"><xsl:value-of select="$x2"/></xsl:if>
				<xsl:if test="$x1 &lt; $x2"><xsl:value-of select="$x1"/></xsl:if>
			</xsl:variable>
			<xsl:variable name="maxx">
				<xsl:if test="$x1 >=   $x2"><xsl:value-of select="$x1"/></xsl:if>
				<xsl:if test="$x1 &lt; $x2"><xsl:value-of select="$x2"/></xsl:if>
			</xsl:variable>
			<xsl:variable name="miny">
				<xsl:if test="$y1 >=   $y2"><xsl:value-of select="$y2"/></xsl:if>
				<xsl:if test="$y1 &lt; $y2"><xsl:value-of select="$y1"/></xsl:if>
			</xsl:variable>
			<xsl:variable name="maxy">
				<xsl:if test="$y1 >=   $y2"><xsl:value-of select="$y1"/></xsl:if>
				<xsl:if test="$y1 &lt; $y2"><xsl:value-of select="$y2"/></xsl:if>
			</xsl:variable>
			
			<xsl:variable name="deltax"><xsl:value-of select="abs(number($maxx) - number($minx))"/></xsl:variable>
			<xsl:variable name="deltay"><xsl:value-of select="abs(number($maxy) - number($miny))"/></xsl:variable>
			
			<xsl:variable name="scalex"><xsl:value-of select="$width  div $deltax"></xsl:value-of></xsl:variable>
			<xsl:variable name="scaley"><xsl:value-of select="$height div $deltay"></xsl:value-of></xsl:variable>
			
			<xsl:variable name="scale">
				<xsl:if test="$scalex >= $scaley">
					<xsl:value-of select="$scaley"/>
				</xsl:if>
				<xsl:if test="$scalex &lt; $scaley">
					<xsl:value-of select="$scalex"/>
				</xsl:if>
			</xsl:variable>
			
			<xsl:text disable-output-escaping="yes">
				&lt;!-- debug information: mode course</xsl:text>
			<xsl:text>
				min_lat: </xsl:text><xsl:value-of select="$min_lat"/>
			<xsl:text>
				max_lat: </xsl:text><xsl:value-of select="$max_lat"/>
			<xsl:text>
				min_lon: </xsl:text><xsl:value-of select="$min_lon"/>
			<xsl:text>
				max_lon: </xsl:text><xsl:value-of select="$max_lon"/>
			<xsl:text>
				mid_lat: </xsl:text><xsl:value-of select="$mid_lat"/>
			<xsl:text>
				mid_lon: </xsl:text><xsl:value-of select="$mid_lon"/>
			<xsl:text>
				deltax: </xsl:text><xsl:value-of select="$deltax"/>
			<xsl:text>
				deltay: </xsl:text><xsl:value-of select="$deltay"/>
			<xsl:text>
				scalex: </xsl:text><xsl:value-of select="$scalex"/>
			<xsl:text>
				scaley: </xsl:text><xsl:value-of select="$scaley"/>
			<xsl:text>
				scale: </xsl:text><xsl:value-of select="$scale"/>
			<xsl:text>
				minx: </xsl:text><xsl:value-of select="$minx"/>
			<xsl:text>
				maxx: </xsl:text><xsl:value-of select="$maxx"/>
			<xsl:text>
				miny: </xsl:text><xsl:value-of select="$miny"/>
			<xsl:text>
				maxy: </xsl:text><xsl:value-of select="$maxy"/>
			<xsl:text>
				minx: </xsl:text><xsl:value-of select="$minx * $scale"/>
			<xsl:text>
				maxx: </xsl:text><xsl:value-of select="$maxx * $scale"/>
			<xsl:text>
				miny: </xsl:text><xsl:value-of select="$miny * $scale"/>
			<xsl:text>
				maxy: </xsl:text><xsl:value-of select="$maxy * $scale"/>
			
					
			<!-- Find teeing area's middle point -->
			<xsl:variable name="rotation-temp">
				<xsl:choose>
					<xsl:when test="$mode = 'hole'">
						<!-- Find green area's middle point -->
						<xsl:variable name="green_lat_min"><xsl:value-of select="min(.//g:placemarks/*[@type='green']/g:position/g:position-gps/g:lat)"/></xsl:variable>
						<xsl:variable name="green_lat_max"><xsl:value-of select="max(.//g:placemarks/*[@type='green']/g:position/g:position-gps/g:lat)"/></xsl:variable>
						<xsl:variable name="green_lon_min"><xsl:value-of select="min(.//g:placemarks/*[@type='green']/g:position/g:position-gps/g:lon)"/></xsl:variable>
						<xsl:variable name="green_lon_max"><xsl:value-of select="max(.//g:placemarks/*[@type='green']/g:position/g:position-gps/g:lon)"/></xsl:variable>
						<xsl:variable name="green_mid_lat">
							<xsl:value-of select="(number($green_lat_max)+number($green_lat_min)) div 2"/>
						</xsl:variable>
						<xsl:variable name="green_mid_lon">
							<xsl:value-of select="(number($green_lon_max)+number($green_lon_min)) div 2"/>
						</xsl:variable>	
						<xsl:variable name="green_mid">
							<xsl:call-template name="Project">
								<xsl:with-param name="lat"><xsl:value-of select="$green_mid_lat"/></xsl:with-param>
								<xsl:with-param name="lon"><xsl:value-of select="$green_mid_lon"/></xsl:with-param>
								<xsl:with-param name="midlat"><xsl:value-of select="$mid_lat"/></xsl:with-param>
								<xsl:with-param name="midlon"><xsl:value-of select="$mid_lon"/></xsl:with-param>
								<xsl:with-param name="scale"><xsl:value-of select="number(1)"/></xsl:with-param>
							</xsl:call-template>
						</xsl:variable>
						<xsl:variable name="green_mid_x"><xsl:value-of select="substring-before($green_mid, ',')"/></xsl:variable>
						<xsl:variable name="green_mid_y"><xsl:value-of select="substring-after($green_mid, ',')"/></xsl:variable>
						
						<xsl:text>
							Green
							green_lat_min: </xsl:text><xsl:value-of select="$green_lat_min"/>
						<xsl:text>
							green_lat_max: </xsl:text><xsl:value-of select="$green_lat_max"/>
						<xsl:text>
							green_lon_min: </xsl:text><xsl:value-of select="$green_lon_min"/>
						<xsl:text>
							green_lon_max: </xsl:text><xsl:value-of select="$green_lon_max"/>
						<xsl:text>
							green_mid_lat: </xsl:text><xsl:value-of select="$green_mid_lat"/>
						<xsl:text>
							green_mid_lon: </xsl:text><xsl:value-of select="$green_mid_lon"/>
						<xsl:text>
							green_mid_x: </xsl:text><xsl:value-of select="$green_mid_x"/>
						<xsl:text>
							green_mid_y: </xsl:text><xsl:value-of select="$green_mid_y"/>
						
						<!-- Find teeing area's middle point -->
						<xsl:variable name="tee_lat_min"><xsl:value-of select="min(.//g:placemarks/*[@type='tee']/g:position/g:position-gps/g:lat)"/></xsl:variable>
						<xsl:variable name="tee_lat_max"><xsl:value-of select="max(.//g:placemarks/*[@type='tee']/g:position/g:position-gps/g:lat)"/></xsl:variable>
						<xsl:variable name="tee_lon_min"><xsl:value-of select="min(.//g:placemarks/*[@type='tee']/g:position/g:position-gps/g:lon)"/></xsl:variable>
						<xsl:variable name="tee_lon_max"><xsl:value-of select="max(.//g:placemarks/*[@type='tee']/g:position/g:position-gps/g:lon)"/></xsl:variable>
						<xsl:variable name="tee_mid_lat">
							<xsl:value-of select="(number($tee_lat_max)+number($tee_lat_min)) div 2"/>
						</xsl:variable>
						<xsl:variable name="tee_mid_lon">
							<xsl:value-of select="(number($tee_lon_max)+number($tee_lon_min)) div 2"/>
						</xsl:variable>
						<xsl:variable name="tee_mid">
							<xsl:call-template name="Project">
								<xsl:with-param name="lat"><xsl:value-of select="$tee_mid_lat"/></xsl:with-param>
								<xsl:with-param name="lon"><xsl:value-of select="$tee_mid_lon"/></xsl:with-param>
								<xsl:with-param name="midlat"><xsl:value-of select="$mid_lat"/></xsl:with-param>
								<xsl:with-param name="midlon"><xsl:value-of select="$mid_lon"/></xsl:with-param>
								<xsl:with-param name="scale"><xsl:value-of select="number(1)"/></xsl:with-param>
							</xsl:call-template>
						</xsl:variable>
						<xsl:variable name="tee_mid_x"><xsl:value-of select="substring-before($tee_mid, ',')"/></xsl:variable>
						<xsl:variable name="tee_mid_y"><xsl:value-of select="substring-after($tee_mid, ',')"/></xsl:variable>
						
						<xsl:text>
							Tee
							tee_lat_min: </xsl:text><xsl:value-of select="$tee_lat_min"/>
						<xsl:text>
							tee_lat_max: </xsl:text><xsl:value-of select="$tee_lat_max"/>
						<xsl:text>
							tee_lon_min: </xsl:text><xsl:value-of select="$tee_lon_min"/>
						<xsl:text>
							tee_lon_max: </xsl:text><xsl:value-of select="$tee_lon_max"/>
						<xsl:text>
							tee_mid_lat: </xsl:text><xsl:value-of select="$tee_mid_lat"/>
						<xsl:text>
							tee_mid_lon: </xsl:text><xsl:value-of select="$tee_mid_lon"/>
						<xsl:text>
							tee_mid_x: </xsl:text><xsl:value-of select="$tee_mid_x"/>
						<xsl:text>
							tee_mid_y: </xsl:text><xsl:value-of select="$tee_mid_y"/>
						<xsl:text disable-output-escaping="yes">
							x1&gt;x2: </xsl:text><xsl:choose>
								<xsl:when test="$tee_mid_x > $green_mid_x">Yes</xsl:when>
								<xsl:otherwise>No</xsl:otherwise>
							</xsl:choose>
						<xsl:text disable-output-escaping="yes">
							y1&gt;y2: </xsl:text><xsl:choose>
								<xsl:when test="$tee_mid_y > $green_mid_y">Yes</xsl:when>
								<xsl:otherwise>No</xsl:otherwise>
							</xsl:choose>
						<xsl:text>
							Angle-temp: </xsl:text>
						
						<!-- Find overall orientation and scaling -->
						<xsl:variable name="angle-temp">
							<xsl:call-template name="GetAngle">
								<xsl:with-param name="x1"><xsl:value-of select="$tee_mid_x"/></xsl:with-param>
								<xsl:with-param name="y1"><xsl:value-of select="$tee_mid_y"/></xsl:with-param>
								<xsl:with-param name="x2"><xsl:value-of select="$green_mid_x"/></xsl:with-param>
								<xsl:with-param name="y2"><xsl:value-of select="$green_mid_y"/></xsl:with-param>
							</xsl:call-template>
						</xsl:variable>
						<xsl:value-of select="concat('ROTATION=',$angle-temp,',x1=',$tee_mid_x,',y1=',$tee_mid_y,',x2=',$green_mid_x,',y2=',$green_mid_y)"></xsl:value-of>
					</xsl:when>
					<xsl:otherwise>
						<xsl:value-of select="string('ROTATION=0,x1=0,y1=0,x2=0,y2=0')"/><!-- no rotation -->
					</xsl:otherwise>
				</xsl:choose>
			</xsl:variable>
			
			<xsl:variable name="rotation"><xsl:value-of select="substring-before(substring-after($rotation-temp, 'ROTATION='), ',')"></xsl:value-of></xsl:variable>
			
			<xsl:variable name="scaling">
				<xsl:call-template name="GetScale">
					<xsl:with-param name="angle"><xsl:value-of select="$rotation"/></xsl:with-param>
				</xsl:call-template>
			</xsl:variable>
			
			<xsl:text>MODIFICATION
			</xsl:text>
			<xsl:text>
				rotation: </xsl:text><xsl:value-of select="$rotation"/>
			<xsl:text>
				scaling: </xsl:text><xsl:value-of select="$scaling"/>
			<xsl:text>
				x,y: </xsl:text><xsl:value-of select="$rotation-temp"/>
			<xsl:text disable-output-escaping="yes">
				--&gt;
			</xsl:text>
			

			<xsl:element name="g">
				<xsl:attribute name="transform">
					<!-- The Gnomonic projection reverses the X axis. We reverse it by applying a SCALE of -1 along the x axis only.
						 The projection is centered, so we TRANSLATE the center in the middle of the drawing area as well.
						 The rotation is not null for hole only, to present the tee at the bottom of the page and green at top.
					  -->
					<xsl:value-of select="concat('translate(',$width div 2, ',', $height div 2,') scale(',-$scaling,',',$scaling,') rotate(',$rotation,')')" />
				</xsl:attribute>
				
				<xsl:apply-templates select=".//g:placemarks" mode="aoi">
					<xsl:with-param name="midlat"><xsl:value-of select="$mid_lat"/></xsl:with-param>
					<xsl:with-param name="midlon"><xsl:value-of select="$mid_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
					<xsl:with-param name="list-of-types"><xsl:value-of select="$typelist"/></xsl:with-param>
				</xsl:apply-templates>
				
				<xsl:apply-templates select=".//g:placemarks" mode="poi">
					<xsl:with-param name="midlat"><xsl:value-of select="$mid_lat"/></xsl:with-param>
					<xsl:with-param name="midlon"><xsl:value-of select="$mid_lon"/></xsl:with-param>
					<xsl:with-param name="scale" ><xsl:value-of select="$scale"/></xsl:with-param>
					<xsl:with-param name="list-of-types"><xsl:value-of select="$typelist"/></xsl:with-param>
				</xsl:apply-templates>

				<xsl:if test="$mode = 'hole'">
					<!-- add marker at tee and green center middle position for testing purpose -->
					<xsl:variable name="x1"><xsl:value-of select="substring-before(substring-after($rotation-temp,'x1='),',')"/></xsl:variable>
					<xsl:variable name="y1"><xsl:value-of select="substring-before(substring-after($rotation-temp,'y1='),',')"/></xsl:variable>
					<xsl:element name="use">
						<xsl:attribute name="xlink:href">#TeeSet</xsl:attribute>
						<xsl:attribute name="x"><xsl:value-of select="$x1 * $scale"/></xsl:attribute>
						<xsl:attribute name="y"><xsl:value-of select="$y1 * $scale"/></xsl:attribute>
						<xsl:attribute name="class">tees</xsl:attribute>
						<xsl:attribute name="transform">
							<xsl:value-of select="concat('rotate(',-$rotation,' ',$x1 * $scale,' ',$y1 * $scale,')')"/>
						</xsl:attribute>
					</xsl:element>
					
					<xsl:variable name="x2"><xsl:value-of select="substring-before(substring-after($rotation-temp,'x2='),',')"/></xsl:variable>
					<xsl:variable name="y2"><xsl:value-of select="substring-after($rotation-temp,'y2=')"/></xsl:variable>
					<xsl:element name="use">
						<xsl:attribute name="xlink:href">#Flag</xsl:attribute>
						<xsl:attribute name="x"><xsl:value-of select="$x2 * $scale"/></xsl:attribute>
						<xsl:attribute name="y"><xsl:value-of select="$y2 * $scale"/></xsl:attribute>
						<xsl:attribute name="transform">
							<xsl:value-of select="concat('rotate(',-$rotation,' ',$x2 * $scale,' ',$y2 * $scale,')')"/>
						</xsl:attribute>
					</xsl:element>
				</xsl:if>		
			</xsl:element><!-- g -->
			
			<xsl:element name="use">
				<xsl:attribute name="xlink:href">#Compass</xsl:attribute>
				<xsl:attribute name="x"><xsl:value-of select="$compassx"/></xsl:attribute>
				<xsl:attribute name="y"><xsl:value-of select="$compassy"/></xsl:attribute>
				<xsl:attribute name="transform">
					<xsl:value-of select="concat('rotate(',$rotation,' ',$compassx,' ',$compassy,')')"/>
				</xsl:attribute>
			</xsl:element>
			
			<xsl:element name="use">
				<xsl:attribute name="xlink:href">#GolfMLLogoSmall</xsl:attribute>
				<xsl:attribute name="x">10</xsl:attribute>
				<xsl:attribute name="y">575</xsl:attribute>
			</xsl:element>
			
		</xsl:element><!-- svg -->
	</xsl:template>
	
	
	<xsl:template match="g:placemarks" mode="aoi">
		<xsl:param name="midlat"/>
		<xsl:param name="midlon"/>
		<xsl:param name="scale"/>
		<xsl:param name="list-of-types"/>

		<xsl:apply-templates select=".//g:aoi[@type=substring-before($list-of-types, ' ')]" mode="draw">
			<xsl:with-param name="midlat"><xsl:value-of select="$midlat"/></xsl:with-param>
			<xsl:with-param name="midlon"><xsl:value-of select="$midlon"/></xsl:with-param>
			<xsl:with-param name="scale"><xsl:value-of select="$scale"/></xsl:with-param>
		</xsl:apply-templates>

		<xsl:if test="string-length(substring-after($list-of-types, ' '))> 2"> <!-- CHECK TEST -->
			<xsl:apply-templates select="." mode="aoi">
				<xsl:with-param name="midlat"><xsl:value-of select="$midlat"/></xsl:with-param>
				<xsl:with-param name="midlon"><xsl:value-of select="$midlon"/></xsl:with-param>
				<xsl:with-param name="scale"><xsl:value-of select="$scale"/></xsl:with-param>
				<xsl:with-param name="list-of-types"><xsl:value-of select="substring-after($list-of-types, ' ')"/></xsl:with-param>
			</xsl:apply-templates>
		</xsl:if>
	</xsl:template>


	<xsl:template match="g:placemarks" mode="poi">
		<xsl:param name="midlat"/>
		<xsl:param name="midlon"/>
		<xsl:param name="scale"/>
		<xsl:param name="list-of-types"/>
		
		<xsl:apply-templates select=".//g:poi[@type=substring-before($list-of-types, ' ')]" mode="draw">
			<xsl:with-param name="midlat"><xsl:value-of select="$midlat"/></xsl:with-param>
			<xsl:with-param name="midlon"><xsl:value-of select="$midlon"/></xsl:with-param>
			<xsl:with-param name="scale"><xsl:value-of select="$scale"/></xsl:with-param>
		</xsl:apply-templates>
		
		<xsl:if test="string-length(substring-after($list-of-types, ' '))> 2"> <!-- CHECK TEST -->
			<xsl:apply-templates select="." mode="poi">
				<xsl:with-param name="midlat"><xsl:value-of select="$midlat"/></xsl:with-param>
				<xsl:with-param name="midlon"><xsl:value-of select="$midlon"/></xsl:with-param>
				<xsl:with-param name="scale"><xsl:value-of select="$scale"/></xsl:with-param>
				<xsl:with-param name="list-of-types"><xsl:value-of select="substring-after($list-of-types, ' ')"/></xsl:with-param>
			</xsl:apply-templates>
		</xsl:if>
	</xsl:template>
	
	
	<xsl:template match="g:poi" mode="draw">
		<xsl:param name="midlat"/>
		<xsl:param name="midlon"/>
		<xsl:param name="scale"/>
		<xsl:element name="use">
			<xsl:apply-templates select="g:position/g:position-gps" mode="xy-projection">
				<xsl:with-param name="scale"><xsl:value-of select="$scale"/></xsl:with-param>
				<xsl:with-param name="midlat"><xsl:value-of select="$midlat"/></xsl:with-param>
				<xsl:with-param name="midlon"><xsl:value-of select="$midlon"/></xsl:with-param>
			</xsl:apply-templates>
			<xsl:attribute name="xlink:href">#Dogleg</xsl:attribute>
		</xsl:element>
	</xsl:template>

	
	<xsl:template match="g:aoi" mode="draw">
		<xsl:param name="midlat"/>
		<xsl:param name="midlon"/>
		<xsl:param name="scale"/>
		<xsl:element name="path">
			<xsl:attribute name="class"><xsl:value-of select="@type"/></xsl:attribute>
			<xsl:attribute name="d"><xsl:text>M </xsl:text>
				<xsl:for-each select="g:position">
					<xsl:sort select="@number" data-type="number"/>
					<xsl:if test="position() = 2"><xsl:text>L </xsl:text></xsl:if>
					<xsl:apply-templates select="g:position-gps" mode="pair-projection">
						<xsl:with-param name="scale"><xsl:value-of select="$scale"/></xsl:with-param>
						<xsl:with-param name="midlat"><xsl:value-of select="$midlat"/></xsl:with-param>
						<xsl:with-param name="midlon"><xsl:value-of select="$midlon"/></xsl:with-param>
					</xsl:apply-templates>
				</xsl:for-each><xsl:text>Z</xsl:text>
			</xsl:attribute>
		</xsl:element>		
	</xsl:template>
		
	
	<xsl:template match="g:position-gps" mode="xy-projection">
		<xsl:param name="midlat"/>
		<xsl:param name="midlon"/>
		<xsl:param name="scale"/>
		
		<xsl:variable name="temp">
			<xsl:call-template name="Project">
				<xsl:with-param name="scale"><xsl:value-of select="$scale"/></xsl:with-param>
				<xsl:with-param name="midlat"><xsl:value-of select="$midlat"/></xsl:with-param>
				<xsl:with-param name="midlon"><xsl:value-of select="$midlon"/></xsl:with-param>
				<xsl:with-param name="lat"><xsl:value-of select="g:lat"/></xsl:with-param>
				<xsl:with-param name="lon"><xsl:value-of select="g:lon"/></xsl:with-param>
			</xsl:call-template>
		</xsl:variable>
		
		<xsl:attribute name="x"><xsl:value-of select="substring-before($temp, ',')"/></xsl:attribute>
		<xsl:attribute name="y"><xsl:value-of select="substring-after( $temp, ',')"/></xsl:attribute>
	</xsl:template>


	<xsl:template match="g:position-gps" mode="pair-projection">
		<xsl:param name="midlat"/>
		<xsl:param name="midlon"/>
		<xsl:param name="scale"/>
		
		<xsl:call-template name="Project">
			<xsl:with-param name="scale"><xsl:value-of select="$scale"/></xsl:with-param>
			<xsl:with-param name="midlat"><xsl:value-of select="$midlat"/></xsl:with-param>
			<xsl:with-param name="midlon"><xsl:value-of select="$midlon"/></xsl:with-param>
			<xsl:with-param name="lat"><xsl:value-of select="g:lat"/></xsl:with-param>
			<xsl:with-param name="lon"><xsl:value-of select="g:lon"/></xsl:with-param>
		</xsl:call-template>
	</xsl:template>


	<xsl:template name="Project" xmlns:Math="java:java.lang.Math">
		<!-- Notes on geometry.
			 Performs a local gnomonic projection around the middle point of the area (midlat,midlon).
			 See: http://mathworld.wolfram.com/GnomonicProjection.html for formulae.
			 See also: http://trac.osgeo.org/proj/ for software, description of projections, and more.
			 For a golf course, since projection is very local, errors in distance are really minimal.
			 Google Earth provides us with DEGREES that we need to convert into GRADIENTS (RAD = DEG * PI / 180).
			 Notes: To measure distance: 1 minute of latitude = 1852m. 1 minute of longitude = 1852m x cos(longitude).
			        We assume cos(mid_longitude) for all distances on the golf course.
		-->
		<xsl:param name="midlat"/>
		<xsl:param name="midlon"/>
		<xsl:param name="lat"/>
		<xsl:param name="lon"/>
		<xsl:param name="scale"/>
		
		<xsl:variable name="cosc">
			<xsl:value-of select="Math:sin(xs:double($lat) * Math:PI() div 180) * Math:sin(xs:double($midlat) * Math:PI() div 180) + Math:cos(xs:double($lat) * Math:PI() div 180) * Math:cos(xs:double($midlat) * Math:PI() div 180) * Math:cos(xs:double($midlon - $lon) * Math:PI() div 180)"/>
		</xsl:variable>
		
		<!--x=--><xsl:value-of select="$scale * (Math:cos(xs:double($midlat) * Math:PI() div 180) * Math:sin(xs:double($midlon - $lon) * Math:PI() div 180) div xs:double($cosc) )"/>
		<xsl:text>,</xsl:text>
		<!--y= --><xsl:value-of select="$scale * (Math:cos(xs:double($lat) * Math:PI() div 180) * Math:sin(xs:double($midlat) * Math:PI() div 180) - Math:sin(xs:double($lat) * Math:PI() div 180) * Math:cos(xs:double($midlat) * Math:PI() div 180) * Math:cos(xs:double($midlon - $lon) * Math:PI() div 180) div $cosc )"/>
		<xsl:text> </xsl:text>
	</xsl:template>
	
	
	<xsl:template name="GetAngle" xmlns:Math="java:java.lang.Math">
		<xsl:param name="x1"/><!-- tee -->
		<xsl:param name="y1"/>
		<xsl:param name="x2"/><!-- green -->
		<xsl:param name="y2"/>
		
		<xsl:variable name="angle">
			<xsl:value-of select="Math:acos( ($y2 - $y1) div Math:sqrt( ($x1 - $x2)*($x1 - $x2) + ($y1 - $y2)*($y1 - $y2) ) ) * 180 div Math:PI()"></xsl:value-of>
		</xsl:variable>
		<xsl:choose>
			<xsl:when test="$x1 > $x2">
				<xsl:value-of select="-$angle + 180"/>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="$angle - 180"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	
	
	<xsl:template name="GetScale" xmlns:Math="java:java.lang.Math">
		<!-- returns the max(sin(angle),cos(angle)) -->
		<xsl:param name="angle"/>
		<xsl:value-of select="Math:max(Math:abs(Math:sin($angle * Math:PI() div 180)),Math:abs(Math:cos($angle * Math:PI() div 180)))"></xsl:value-of>
	</xsl:template>
	

	<xsl:template name="Defs">
		<xsl:param name="scale"/>
			<defs>
				<xsl:if test="$scale"><!-- does not work as expected... -->
					<xsl:attribute name="transform"><xsl:value-of select="concat('scale(',$scale,')')"></xsl:value-of></xsl:attribute>
				</xsl:if>
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

.distance-marker-100,
.distance-marker-close {
	fill: #44f;
	stroke: #00f;
	stroke-width: 1px;
}

.distance-marker-150,
.distance-marker-135,
.distance-marker,
.distance-marker-mid {
	fill: #f44;
	stroke: #f00;
	stroke-width: 1px;
}

.distance-marker-200,
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

.neutral {
	fill: none;
	stroke: #888;
	stroke-width: 1px;
}


/* Classes for special decorative objects
 *
 */
.compass-center {
	fill: #ddd;
	stroke: #fff;
	stroke-width: 1px;
}
.compass-needle {
	fill: #ddd;
	stroke: #fff;
	stroke-width: 1px;
}
.compass-north {
	fill: #fff;
}
.flag-pole {
	fill: none;
	stroke: #f00;
	stroke-width: 2px;
}
.flag-flag {
	fill: #ff0;
	stroke: #f00;
	stroke-width: 1px;
}
.flag-cup {
	fill: #fff;
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
				
		<g id="Aim">
			<circle r="4"/>
		</g>
		
		<g id="Hole">
			<circle r="2"/>
		</g>
				
		<g id="Flag" transform="translate(0 -30) scale(-0.2 0.2)">
			<line id="FlagPole" class="flag-pole" x1="0.5" y1="0" x2="0.5" y2="150"/>
			<circle id="FlagCup" class="flag-cup" r="10"  cx="0" cy="145" />
			<g id="FlagFlag">
				<path class="flag-flag"
					d="M55.339,45.968c-18.28-14.516-36.559,14.516-54.839,0c0-12.097,0-24.193,0-36.29
					c18.28,14.516,36.559-14.516,54.839,0C55.339,21.774,55.339,33.871,55.339,45.968z"/>
			</g>
		</g>
				
		<g id="Compass">
			<polygon id="CompassNeedle" class="compass-needle" points="0,30 -8,0 0,-20 8,0"/>
			<circle id="CompassCenter" class="compass-center" r="10"/>
			<text id="CompassNorth" class="compass-north" x="-4.7" y="40">N</text>
		</g>
				
		<g id="GolfMLLogoSmall">
			<image xlink:href="../stylesheets/images/golfml-small.png"></image>
		</g>
				
	</defs>
	</xsl:template>

</xsl:stylesheet>