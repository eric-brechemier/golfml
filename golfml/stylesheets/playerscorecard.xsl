<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:g="http://code.google.com/p/golfml">
<!--
	DESCRIPTION
				Demonstation stableford scorecard for a player
				using the course definition and a player definition
	
	HISTORY
				V0.9 May 2012
				Using parameters for tee-colour
				Using parameters for length units
				Hard-coded country-club (first one)
				Hard-coded golf-course (first one)
				Hard-coded player (first one)
	TODO
				Use <application> data to eliminate params and hard-coding
	AUTHOR
				minesadorada@charcodelvalle.com
-->                                
        <xsl:output method="html" indent="yes" encoding="UTF-8"/>
        <xsl:param name="teeparam">yellow</xsl:param>
        <xsl:param name="lengthunits">meters</xsl:param>

		<xsl:variable name="CLUBNAME" select="g:golfml/g:country-club[1]/g:name" />
		<xsl:variable name="PLAYERNAME" select="g:golfml/g:country-club[1]/g:player[1]/g:name" />
		<xsl:variable name="EXACTEGAHANDICAP" select="g:golfml/g:country-club[1]/g:player[1]/g:current-handicap" />
		<xsl:variable name="NUMBEROFHOLES" select="g:golfml/g:country-club[1]/g:golf-course[1]/g:tee-set[@colour=$teeparam]/g:summary/g:holes" />
         <xsl:template match="g:golfml/g:country-club[1]">
                <html>
                        <head>
                                <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
                                <link href="golfml.css" rel="stylesheet" type="text/css"/>
                                <title>
									GolfML Scorecard for <xsl:value-of select="$PLAYERNAME"/> 
								</title>
                        </head>
                        <body>
                                <fieldset>
								<legend>golfml Scorecard</legend>
                                <h1><xsl:value-of select="$CLUBNAME" /></h1>
								<h4 align='center'>
									<xsl:value-of select="g:address/g:municipality"/>,
									<xsl:value-of select="g:address/g:region"/>,
									<xsl:value-of select="g:address/g:country"/>
									<br/>
									<xsl:element name="a">
										<xsl:attribute name="href">
											<xsl:value-of select="g:address/g:website"/>
										</xsl:attribute>
											<xsl:value-of select="g:address/g:website"/>
									</xsl:element>
									&#160;&#160;Phone: <xsl:value-of select="g:contact/g:phone"/>	
 								</h4>
                                <h3 align='center'>
									Player: <xsl:value-of select="g:player[1]/g:name"/>
									(<xsl:value-of select="g:player[1]/@gender"/>)
									EGA Exact: <xsl:value-of select="$EXACTEGAHANDICAP"/>
								</h3>
								<p align='center'>
								<xsl:apply-templates select="g:golf-course[1]"/>
								</p>
								</fieldset>
                        </body>
                </html>
        </xsl:template>
		<xsl:template match="g:golf-course[1]">
			<xsl:variable name="COURSENAME" select="g:name"/>
			<xsl:value-of select="$COURSENAME"/>&#160;&#160;
			<xsl:variable name="COURSERATING" select="g:tee-set[@colour=$teeparam]/g:qualification/g:qualification-usga/g:rating" /> 
			<xsl:variable name="SLOPERATING" select="g:tee-set[@colour=$teeparam]/g:qualification/g:qualification-usga/g:slope" />
			<xsl:variable name="COURSEPAR" select="sum(g:tee-set[@colour=$teeparam]/g:tee/g:par)"/>
			<!--(EGAhandicap * (SlopeRating/113)) + (CourseRating - CoursePar)-->
			<xsl:variable name="PLAYINGHANDICAP" select="round(((number($EXACTEGAHANDICAP) * number($SLOPERATING div 113)) + number($COURSERATING - $COURSEPAR)))"/> 
			
			<xsl:for-each select="g:tee-set[@colour=$teeparam]">
				<xsl:variable name="TEESETNAME" select="@name"/>
				<xsl:value-of select="@name"/> Tees (<xsl:value-of select="@colour"/>)
					<br/>
					Course Rating: <xsl:value-of select="$COURSERATING"/>,
					Slope Rating: <xsl:value-of select="$SLOPERATING"/>,
					Number of Holes: <xsl:value-of select="$NUMBEROFHOLES" />,
					Par: <xsl:value-of select="$COURSEPAR"/>
					Length: <xsl:value-of select="sum(g:tee/g:length[@units=$lengthunits])"/>
					<xsl:value-of select="substring($lengthunits,1,1)" />
					<br/><b>Playing Handicap for <xsl:value-of select="$PLAYERNAME"/> on <xsl:value-of select="$COURSENAME"/>,&#160;<xsl:value-of select="$TEESETNAME"/> tees = <xsl:value-of select="$PLAYINGHANDICAP"/></b>
			</xsl:for-each>
			<p/>		
			<xsl:variable name="CR" select="g:tee-set[@colour=$teeparam]/g:qualification/g:qualification-usga/g:rating" /> 
			<xsl:variable name="SR" select="g:tee-set[@colour=$teeparam]/g:qualification/g:qualification-usga/g:slope" />
			<xsl:variable name="CP" select="sum(g:tee-set[@colour=$teeparam]/g:tee/g:par)"/>
			<xsl:variable name="PHCP" select="round(((number($EXACTEGAHANDICAP) * number($SR div 113)) + number($CR - $CP)))"/> 
            <table class="course-data">
				<caption><xsl:value-of select="$CLUBNAME"/>&#160;&#160;<xsl:value-of select="$COURSENAME"/>&#160;&#160;Scorecard</caption>
					<thead>
						<tr>
							<td>
								<xsl:value-of select="$PLAYERNAME"/> 
							</td>
							<xsl:element name="td">
								<xsl:attribute name="colspan"><xsl:value-of select="$NUMBEROFHOLES div 2"/>
							</xsl:attribute>
								Playing Handicap: <xsl:value-of select="$PHCP"/>
							</xsl:element>
							<xsl:element name="td">
								<xsl:attribute name="colspan"><xsl:value-of select="$NUMBEROFHOLES div 2"/>
							</xsl:attribute>
								Date: _______________________________________
							</xsl:element>
						</tr>
						<tr id="header">
							<td>Hole</td>
							<xsl:for-each select="g:tee-set[@colour=$teeparam]/g:tee">
									<xsl:sort select="@number" data-type="number"/>
									<td>                                            
											<xsl:value-of select="position()"/>
									</td>
							</xsl:for-each>
						</tr>
					</thead>
					<tbody>
						<xsl:element name="tr">
						<xsl:attribute name="class"><xsl:value-of select="concat('tee-',$teeparam)"/></xsl:attribute>
							<td class="label">
								S.I.
							</td>
							<xsl:for-each select="g:tee-set[@colour=$teeparam]/g:tee">
									<td>
											<xsl:value-of select="g:handicap-stroke"/>
									</td>
							</xsl:for-each>
						</xsl:element>
						<xsl:element name="tr">
						<xsl:attribute name="class"><xsl:value-of select="concat('tee-',$teeparam)"/></xsl:attribute>
							<td class="label">
								Par
							</td>
							<xsl:for-each select="g:tee-set[@colour=$teeparam]/g:tee">
									<td>
											<b><xsl:value-of select="g:par"/></b>
									</td>
							</xsl:for-each>
						</xsl:element>
						<tr>
							<td class="label">Allowed</td>
							<xsl:for-each select="g:tee-set[@colour=$teeparam]/g:tee">
									<td>
										<xsl:if test="(number($PHCP) - number(g:handicap-stroke) >= 0)">*</xsl:if>
										<xsl:if test="((number($PHCP)-18) - number(g:handicap-stroke) >= 0)">*</xsl:if>
										<xsl:if test="((number($PHCP)-36) - number(g:handicap-stroke) >= 0)">*</xsl:if>
									</td>
							</xsl:for-each>
						</tr>
						<tr>
							<td class="label">Score</td>
							<xsl:for-each select="g:tee-set[@colour=$teeparam]/g:tee">
								<td>&#160;&#160;</td>
							</xsl:for-each>
						</tr>
						<tr>
							<td class="label">Points</td>
							<xsl:for-each select="g:tee-set[@colour=$teeparam]/g:tee">
								<td>&#160;&#160;</td>
							</xsl:for-each>
						</tr>
					</tbody>
            </table>

        </xsl:template>
</xsl:stylesheet>