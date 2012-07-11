<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.1"
				xmlns:xsl="http://www.w3.org/1999/XSL/Transform"	
				xmlns:g="http://code.google.com/p/golfml"
				xmlns:golfml-data="http://code.google.com/p/golfml-data"
	>
<!-- golfml2osm.xsl

DESCRIPTION

	Demonstration stylesheet for GolfML.
	From a golfml Golf Course description, outputs a Open Street Map (OSM) XML file with "Leisure" Layer information for a golf course.
	

SEE ALSO
	http://wiki.openstreetmap.org/wiki/Proposed_features/Golf_course
	http://code.google.com/p/golfml
	http://code.google.com/p/osmgolf
	https://sites.google.com/site/maps4locus/home/


VERSION
	$Revision$


HISTORY
	Jul 2012: Created.

-->
	<xsl:output method="xml" indent="yes" />


	<xsl:template match="/">
		<xsl:apply-templates select="/g:golfml/g:country-club" />
	</xsl:template>


	<xsl:template match="g:country-club">
		<xsl:element name="osm">
			<xsl:attribute name="version">0.6</xsl:attribute>			
			<xsl:attribute name="generator">golfml2osm</xsl:attribute>			
			<xsl:element name="bounds">
				<xsl:attribute name="minlat">0</xsl:attribute>
				<xsl:attribute name="maxlat">0</xsl:attribute>
				<xsl:attribute name="minlon">0</xsl:attribute>
				<xsl:attribute name="maxlon">0</xsl:attribute>
			</xsl:element>
			<xsl:apply-templates select="g:golf-course" />
		</xsl:element>
	</xsl:template>


	<xsl:template match="g:golf-course">
		<xsl:comment>
P A R T   1 :   G O L F   C O U R S E   D A T A
Golf course <xsl:value-of select="g:name"/>.</xsl:comment>
		<xsl:apply-templates select="g:holes" mode="data" />
		<xsl:comment>
P A R T   3 :   G O L F   C O U R S E   R E L A T I O N
3.1. Golf course data for <xsl:value-of select="g:name"/>.
</xsl:comment>
		<xsl:element name="relation">
			<xsl:attribute name="id"><xsl:value-of select="generate-id()"/></xsl:attribute>
			<xsl:call-template name="keqv">
				<xsl:with-param name="k">type</xsl:with-param>
				<xsl:with-param name="v">golf_course</xsl:with-param>
			</xsl:call-template>
			<xsl:call-template name="keqv">
				<xsl:with-param name="k">name</xsl:with-param>
				<xsl:with-param name="v"><xsl:value-of select="g:name"/></xsl:with-param>
			</xsl:call-template>			
			<xsl:apply-templates select="g:tee-set/g:qualification/g:qualification-usga" />
			<xsl:comment>
3.2. Members for holes.
</xsl:comment>
			<xsl:apply-templates select="g:holes" mode="member"/>			<!-- Hole specific elements -->
			<xsl:comment>
3.3. Members for facilities.
</xsl:comment>
			<xsl:apply-templates select="g:holes/g:facilities" mode="member"/>		<!-- General elements for all holes -->
		</xsl:element>
	</xsl:template>
	

	<xsl:template match="g:qualification-usga">
		<xsl:call-template name="keqv">
			<xsl:with-param name="k">cr:<xsl:value-of select="../../@colour"/></xsl:with-param>
			<xsl:with-param name="v"><xsl:value-of select="g:rating"/></xsl:with-param>
		</xsl:call-template>
		<xsl:call-template name="keqv">
			<xsl:with-param name="k">sr:<xsl:value-of select="../../@colour"/></xsl:with-param>
			<xsl:with-param name="v"><xsl:value-of select="g:slope"/></xsl:with-param>
		</xsl:call-template>
	</xsl:template>
	

	<xsl:template match="g:holes" mode="data">
		
		<xsl:comment>
1.1. Golf course data for holes.
</xsl:comment>
		<xsl:apply-templates select="g:hole" mode="data" />				<!-- Hole specific elements -->
		<xsl:comment>
1.2  Golf course data for other elements and facilities.
</xsl:comment>
		<xsl:apply-templates select="g:facilities" mode="data"/>		<!-- General elements for all holes -->
		<xsl:comment>
P A R T   2 :   H O L E   R E L A T I O N S
</xsl:comment>
		<xsl:apply-templates select="g:hole" mode="member" />			<!-- Relations for holes -->
	</xsl:template>


	<xsl:template match="g:holes" mode="member">
		<xsl:apply-templates select="g:hole" mode="member-no-relation" />
	</xsl:template>


	<xsl:template match="g:hole" mode="data">
		<xsl:comment>
1.1.<xsl:value-of select="@number"/>. Hole data for hole <xsl:value-of select="@number"/>.
</xsl:comment>

		<!-- Generates points and areas (nodes and ways) for hole -->
		<xsl:apply-templates select="g:placemarks" mode="data" />
	</xsl:template>


	<xsl:template match="g:hole" mode="member">			<!-- Hole specific elements encapsulated in relation -->
		<xsl:comment>
2.<xsl:value-of select="@number"/>. Hole relation for hole <xsl:value-of select="@number"/>.
</xsl:comment>
		
		<xsl:element name="relation">
			<xsl:attribute name="id"><xsl:value-of select="generate-id()"/></xsl:attribute>					
			
			<!-- Generates golfing data for hole -->
			<xsl:call-template name="keqv">
				<xsl:with-param name="k">golf</xsl:with-param>
				<xsl:with-param name="v">hole</xsl:with-param>
			</xsl:call-template>
			<xsl:call-template name="keqv">
				<xsl:with-param name="k">ref</xsl:with-param>
				<xsl:with-param name="v"><xsl:value-of select="@number"/></xsl:with-param>
			</xsl:call-template>
			<!-- Loops over all tee sets to print their specifics -->
			<xsl:apply-templates select="../../g:tee-set/g:tee[@number=current()/@number]" />
			
			<!-- Generates references to points and areas for hole -->
			<xsl:apply-templates select="g:placemarks" mode="member" />
		</xsl:element>
	</xsl:template>
	
	
	<xsl:template match="g:hole" mode="member-no-relation">
		<xsl:comment>
3.2.<xsl:value-of select="@number"/>. Members for hole <xsl:value-of select="@number"/>.
</xsl:comment>		
		<xsl:apply-templates select="g:placemarks" mode="member" />
	</xsl:template>


	<xsl:template match="g:placemarks" mode="data">
		<xsl:apply-templates select="g:aoi" mode="data" />
		<xsl:apply-templates select="g:poi" mode="data" />
	</xsl:template>
	
	
	<xsl:template match="g:placemarks" mode="member">
		<xsl:apply-templates select="g:aoi" mode="member" />
		<xsl:apply-templates select="g:poi" mode="member" />
	</xsl:template>
	
	
	<xsl:template match="g:aoi" mode="data">
		<xsl:apply-templates select="g:position/g:gps" mode="data" />
		<xsl:element name="way">
			<xsl:attribute name="id"><xsl:value-of select="generate-id()"/></xsl:attribute>
			<xsl:apply-templates select="g:position/g:gps" mode="reference" />
			<xsl:if test="../../@number &gt; 0"> <!-- if aoi assigned to a hole -->
				<xsl:call-template name="keqv">
					<xsl:with-param name="k">ref</xsl:with-param>
					<xsl:with-param name="v"><xsl:value-of select="../../@number"/></xsl:with-param>
				</xsl:call-template>
			</xsl:if>
			<xsl:comment><xsl:value-of select="@type"/></xsl:comment>
			<xsl:call-template name="type_equivalence">
				<xsl:with-param name="golfml_type"><xsl:value-of select="@type"/></xsl:with-param>
			</xsl:call-template>
		</xsl:element>
	</xsl:template>


	<xsl:template match="g:aoi" mode="member">
		<xsl:element name="member">
			<xsl:attribute name="ref"><xsl:value-of select="generate-id()"/></xsl:attribute>
			<xsl:attribute name="type">way</xsl:attribute>
			<xsl:call-template name="addattr" />
			<xsl:comment><xsl:value-of select="@type"/></xsl:comment>
		</xsl:element>				
	</xsl:template>
	
	
	<xsl:template match="g:poi" mode="data">
		<xsl:apply-templates select="g:position/g:gps" mode="data2" />
	</xsl:template>
	
	
	<xsl:template match="g:poi" mode="reference">
		<xsl:apply-templates select="g:position/g:gps" mode="reference" />
	</xsl:template>
	

	<xsl:template match="g:poi" mode="member">
		<xsl:apply-templates select="g:position/g:gps" mode="member" />
	</xsl:template>


	<xsl:template match="g:gps" mode="data">	<!-- Regular (untyped) node part of a way -->
		<xsl:element name="node">
			<xsl:attribute name="id"><xsl:value-of select="generate-id()"/></xsl:attribute>
			<xsl:attribute name="lat"><xsl:value-of select="@lat"/></xsl:attribute>
			<xsl:attribute name="lon"><xsl:value-of select="@lon"/></xsl:attribute>
			<xsl:call-template name="addattr" />
		</xsl:element>
	</xsl:template>
	
	
	<xsl:template match="g:gps" mode="data2">	<!-- Typed node of interest -->
		<xsl:element name="node">
			<xsl:attribute name="id"><xsl:value-of select="generate-id()"/></xsl:attribute>
			<xsl:attribute name="lat"><xsl:value-of select="@lat"/></xsl:attribute>
			<xsl:attribute name="lon"><xsl:value-of select="@lon"/></xsl:attribute>
			<xsl:call-template name="addattr" />
			<xsl:if test="../../../../@number &gt; 0"> <!-- if poi assigned to a hole -->
				<xsl:call-template name="keqv">
					<xsl:with-param name="k">ref</xsl:with-param>
					<xsl:with-param name="v"><xsl:value-of select="../../../../@number"/></xsl:with-param>
				</xsl:call-template>
			</xsl:if>
			<xsl:comment><xsl:value-of select="@type"/></xsl:comment>
			<xsl:call-template name="type_equivalence">
				<xsl:with-param name="golfml_type"><xsl:value-of select="../../@type"/></xsl:with-param>
			</xsl:call-template>			
		</xsl:element>
	</xsl:template>


	<xsl:template match="g:gps" mode="reference">
		<xsl:element name="nd">
			<xsl:attribute name="ref"><xsl:value-of select="generate-id()"/></xsl:attribute>
		</xsl:element>
	</xsl:template>
	

	<xsl:template match="g:gps" mode="member">
		<xsl:element name="member">
			<xsl:attribute name="ref"><xsl:value-of select="generate-id()"/></xsl:attribute>
			<xsl:attribute name="type">node</xsl:attribute>
			<xsl:call-template name="addattr" />
			<xsl:comment>
<xsl:value-of select="../../@type"/></xsl:comment>
		</xsl:element>
	</xsl:template>
	
	
	<xsl:template match="g:facilities" mode="data">			<!-- General elements for all holes -->
		<xsl:apply-templates select="g:placemarks" mode="data" />
	</xsl:template>
	
	
	<xsl:template match="g:facilities" mode="member">		<!-- General elements for all holes -->
		<xsl:apply-templates select="g:placemarks" mode="member" />
	</xsl:template>
	
	
	<xsl:template match="g:tee">
		<xsl:call-template name="keqv">
			<xsl:with-param name="k">par:<xsl:value-of select="../@colour"/></xsl:with-param>
			<xsl:with-param name="v"><xsl:value-of select="g:par"/></xsl:with-param>
		</xsl:call-template>
		<xsl:call-template name="keqv">
			<xsl:with-param name="k">handicap:<xsl:value-of select="../@colour"/></xsl:with-param>
			<xsl:with-param name="v"><xsl:value-of select="g:handicap-stroke"/></xsl:with-param>
		</xsl:call-template>
		<xsl:call-template name="keqv">
			<xsl:with-param name="k">dist:<xsl:value-of select="../@colour"/></xsl:with-param>
			<!--
				 MUST SELECT LENGTH UNIT ACCORDING TO REGION: US, Canada?: Yards,
				 rest of the world: Metric @units="metric|imperial".
			  -->
			<xsl:with-param name="v"><xsl:value-of select="g:length"/></xsl:with-param>
		</xsl:call-template>
	</xsl:template>


	<!--	OSM mandatory fields
	  -->
	<xsl:template name="addattr">
		<xsl:attribute name="userid">golfml</xsl:attribute>
		<xsl:attribute name="userid">golfml</xsl:attribute>
		<xsl:attribute name="userid">golfml</xsl:attribute>
		<xsl:attribute name="userid">golfml</xsl:attribute>
	</xsl:template>

	<!--	Prints k=v attributes for tag elements. I.e., generates <tag k="$k" v="$v" />
	  -->
	<xsl:template name="keqv">
		<xsl:param name="k"/>
		<xsl:param name="v"/>
		<xsl:element name="tag">
			<xsl:attribute name="k"><xsl:value-of select="$k"/></xsl:attribute>
			<xsl:attribute name="v"><xsl:value-of select="$v"/></xsl:attribute>
		</xsl:element>
	</xsl:template>
	

	<!--	Matches GolfML "type" to OSM tag (k,v) pairs.
	  -->
	<xsl:template name="type_equivalence">
		<xsl:param name="golfml_type"/>

		<xsl:call-template name="keqv">
			<xsl:with-param name="k"><xsl:value-of select="document('')/xsl:stylesheet/golfml-data:data/golfml-data:tr[@type=$golfml_type]/@k"/></xsl:with-param>
			<xsl:with-param name="v"><xsl:value-of select="document('')/xsl:stylesheet/golfml-data:data/golfml-data:tr[@type=$golfml_type]/@v"/></xsl:with-param>
		</xsl:call-template>
	</xsl:template>
	
	<golfml-data:data>
		<!--  type="type in golfml"	TAG:k="osm key"		v="osm value" -->
		<golfml-data:tr type="tee"				k="golf"		v="tee" />
		<golfml-data:tr type="fairway"			k="golf"		v="faiway" />
		<golfml-data:tr type="hole"				k="golf"		v="pin" />
		<golfml-data:tr type="green"				k="golf"		v="green" />
		<golfml-data:tr type="fringe"				k="golf"		v="faiway" />
		<golfml-data:tr type="tree"				k="natural"		v="tree" />
		<golfml-data:tr type="trees"				k="natural"		v="wood" />
		<golfml-data:tr type="bush"				k="natural"		v="wood" />
		<golfml-data:tr type="semi-rough"			k="golf"		v="rough" />
		<golfml-data:tr type="rough"				k="golf"		v="rough" />
		<golfml-data:tr type="heavy-rough"		k="golf"		v="rough" />
		<golfml-data:tr type="bunker"				k="golf"		v="bunker" />
		<golfml-data:tr type="trap"				k="golf"		v="bunker" />
		<golfml-data:tr type="greenside-trap"		k="golf"		v="bunker" />
		<golfml-data:tr type="fairway-trap"		k="golf"		v="bunker" />
		<golfml-data:tr type="front-water"		k="golf"		v="water_hazard" />
		<golfml-data:tr type="lateral-water"		k="golf"		v="lateral_water_hazard" />
		<golfml-data:tr type="water"				k="natural"		v="water" />
		<golfml-data:tr type="path"				k="highway"		v="footway" />
		<golfml-data:tr type="building"			k="building"	v="yes" />
		<golfml-data:tr type="obstruction"		k="building"	v="yes" />
		<golfml-data:tr type="out-of-bound"		k="golf"		v="rough" />
		<golfml-data:tr type="hole-contour"		k="golf"		v="fairway" />
		<golfml-data:tr type="aim"				k="golf"		v="aim" />
		<golfml-data:tr type="marker"				k="golf"		v="marker" />
		<golfml-data:tr type="marker-100"			k="golf"		v="marker" />
		<golfml-data:tr type="marker-135"			k="golf"		v="marker" />
		<golfml-data:tr type="marker-150"			k="golf"		v="marker" />
		<golfml-data:tr type="marker-200"			k="golf"		v="marker" />
		<golfml-data:tr type="sprinkler"			k="golf"		v="marker" />
		<golfml-data:tr type="dogleg"				k="golf"		v="aim" />
		<golfml-data:tr type="other"				k="golf"		v="other" />
		<golfml-data:tr type="practice"			k="golf"		v="practice" />
		<golfml-data:tr type="store"				k="building"	v="commercial" />
		<golfml-data:tr type="food"				k="amenity"		v="restaurant" />
		<golfml-data:tr type="corporate"			k="building"	v="commercial" />
		<golfml-data:tr type="bathroom"			k="amenity"		v="toilets" />
		<golfml-data:tr type="water"				k="amenity"		v="drinking_water" />
		<golfml-data:tr type="other"				k="golf"		v="other" />
		<golfml-data:tr type="0"					k="golf"		v="ball_washer" />
		<golfml-data:tr type="0"					k="golf"		v="driving_range" />
	</golfml-data:data>

</xsl:stylesheet>