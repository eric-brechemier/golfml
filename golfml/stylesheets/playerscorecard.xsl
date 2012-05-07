<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" 
xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
xmlns:g="http://code.google.com/p/golfml"
>
<!--
	DESCRIPTION
				Demonstation stableford scorecard for a player
				using the course definition and a player definition
	
	HISTORY
				V0.9 May 2012
				V0.9.1 May 2012
	AUTHOR
				minesadorada@charcodelvalle.com
-->                                
        <xsl:output method="html" indent="yes" encoding="UTF-8"/>
        <xsl:variable name="appname" select="'golfml custom scorecard'" />
        <xsl:variable name="APPSECTION" select="g:golfml/g:application[@name=$appname]" />

		<!-- Set variables from golfmlclass <application> section -->
		<xsl:variable name="TEECOLOUR" select="$APPSECTION/g:tee-colour" />
		<xsl:variable name="PLAYERNAME" select="$APPSECTION/g:player-name" />
		<xsl:variable name="PLAYERNAMESTRING"><xsl:value-of select="$PLAYERNAME"/></xsl:variable>
		<xsl:variable name="COURSENAME" select="$APPSECTION/g:course-name" />
		<xsl:variable name="EXACTEGAHANDICAP" select="$APPSECTION/g:player-handicap" />
		<xsl:variable name="LENGTHUNITS" select="$APPSECTION/g:units" />
		<xsl:variable name="HANDICAPSYSTEM" select="$APPSECTION/g:handicap-system" />
		<xsl:variable name="CLUBNAME" select="g:golfml/g:country-club[1]/g:name" />
        
        <xsl:template match="g:golfml">
			<!--
			<xsl:apply-templates select="g:player[g:name=$PLAYERNAMESTRING]"/>
			-->
			<xsl:apply-templates select="g:country-club[1]"/>
		</xsl:template>

<!--
        <xsl:template match="g:player[g:name=$PLAYERNAMESTRING]">
				<xsl:variable name="PLAYERHANDICAP" select="g:current-handicap"/>
		</xsl:template>
-->			
        <!-- Only the first country club is processed -->
        <xsl:template match="g:country-club[1]">
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
								<legend>golfml CourseWriter Scorecard</legend>
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
									Player: <xsl:value-of select="$PLAYERNAME"/>
									EGA Exact: <xsl:value-of select="$EXACTEGAHANDICAP"/>
								</h3>
								<p align='center'>
									<xsl:apply-templates select="g:golf-course"/>
								</p>
								</fieldset>
                        </body>
                </html>
        </xsl:template>
		<xsl:template match="g:golf-course">
			<xsl:if test="g:name=$COURSENAME">
					<!-- Set variables for this golf course -->
					<xsl:variable name="NUMBEROFHOLES" select="g:tee-set[@colour=$TEECOLOUR]/g:summary/g:holes" />
					<xsl:variable name="COURSERATING" select="g:tee-set[@colour=$TEECOLOUR]/g:qualification/g:qualification-usga/g:rating" /> 
					<xsl:variable name="SLOPERATING" select="g:tee-set[@colour=$TEECOLOUR]/g:qualification/g:qualification-usga/g:slope" />
					<xsl:variable name="COURSEPAR" select="sum(g:tee-set[@colour=$TEECOLOUR]/g:tee/g:par)"/>
					<!--EGA Formula for Playing handicap is (ExactEGAhandicap * (SlopeRating/113)) + (CourseRating - CoursePar)-->
					<xsl:variable name="PLAYINGHANDICAP" select="round(((number($EXACTEGAHANDICAP) * number($SLOPERATING div 113)) + number($COURSERATING - $COURSEPAR)))"/> 
					<xsl:variable name="EXACTPLAYINGHANDICAP" select="((number($EXACTEGAHANDICAP) * number($SLOPERATING div 113)) + number($COURSERATING - $COURSEPAR))"/> 

					<xsl:value-of select="$COURSENAME"/>&#160;&#160;
					<xsl:for-each select="g:tee-set[@colour=$TEECOLOUR]">
						<!-- Set variables for this tee-set -->
						<xsl:variable name="TEESETNAME" select="@name"/>
						<xsl:value-of select="@name"/> Tees (<xsl:value-of select="@colour"/>)
							<br/>
							Course Rating: <xsl:value-of select="$COURSERATING"/>,
							Slope Rating: <xsl:value-of select="$SLOPERATING"/>,
							Number of Holes: <xsl:value-of select="$NUMBEROFHOLES" />,
							Par: <xsl:value-of select="$COURSEPAR"/>
							Length: <xsl:value-of select="sum(g:tee/g:length[@units=$LENGTHUNITS])"/>
							<xsl:value-of select="substring($LENGTHUNITS,1,1)" />
							<br/>
							(<xsl:value-of select="$EXACTEGAHANDICAP"/> * (<xsl:value-of select="$SLOPERATING"/>/113) + (<xsl:value-of select="$COURSERATING"/> - <xsl:value-of select="$COURSEPAR"/>) = <xsl:value-of select="$EXACTPLAYINGHANDICAP"/>)
							<br/>
							<b>Playing Handicap for <xsl:value-of select="$PLAYERNAME"/> on 
							<xsl:value-of select="$COURSENAME"/>,&#160;<xsl:value-of select="$TEESETNAME"/> tees
							 = <xsl:value-of select="$PLAYINGHANDICAP"/></b>
					</xsl:for-each>
					<p/>		
					<xsl:variable name="CR" select="g:tee-set[@colour=$TEECOLOUR]/g:qualification/g:qualification-usga/g:rating" /> 
					<xsl:variable name="SR" select="g:tee-set[@colour=$TEECOLOUR]/g:qualification/g:qualification-usga/g:slope" />
					<xsl:variable name="CP" select="sum(g:tee-set[@colour=$TEECOLOUR]/g:tee/g:par)"/>
					<xsl:variable name="PHCP" select="round(((number($EXACTEGAHANDICAP) * number($SR div 113)) + number($CR - $CP)))"/> 
					<table width="80%" align="center">
						<caption><xsl:value-of select="$CLUBNAME"/>&#160;-&#160;<xsl:value-of select="$COURSENAME"/>&#160;&#160;&#160;&#160;Date:_____________________________</caption>
							<thead>
								<tr  align="right">
									<td>
										<xsl:value-of select="$PLAYERNAME"/> 
									</td>
									<xsl:element name="td">
										<xsl:attribute name="colspan"><xsl:value-of select="$NUMBEROFHOLES"/></xsl:attribute>
									Playing Handicap: <xsl:value-of select="$PHCP"/>
									</xsl:element>
								</tr>
							</thead>
							<tbody>
								<tr id="header">
									<td>Hole</td>
									<xsl:for-each select="g:tee-set[@colour=$TEECOLOUR]/g:tee">
											<xsl:sort select="@number" data-type="number"/>
											<td>                                            
													<xsl:value-of select="position()"/>
											</td>
									</xsl:for-each>
								</tr>
								<xsl:element name="tr">
								<xsl:attribute name="class"><xsl:value-of select="concat('tee-',$TEECOLOUR)"/></xsl:attribute>
									<td class="label">
										<xsl:if test="$LENGTHUNITS='meters'">
											<xsl:text>Meters</xsl:text>
										</xsl:if>	
										<xsl:if test="$LENGTHUNITS='yards'">
											<xsl:text>Yards</xsl:text>
										</xsl:if>	
									</td>
									<xsl:for-each select="g:tee-set[@colour=$TEECOLOUR]/g:tee">
											<td>
													<xsl:value-of select="g:length[@units=$LENGTHUNITS]"/>
											</td>
									</xsl:for-each>
								</xsl:element>
								<xsl:element name="tr">
								<xsl:attribute name="class"><xsl:value-of select="concat('tee-',$TEECOLOUR)"/></xsl:attribute>
									<td class="label">
										S.I.
									</td>
									<xsl:for-each select="g:tee-set[@colour=$TEECOLOUR]/g:tee">
											<td>
													<xsl:value-of select="g:handicap-stroke"/>
											</td>
									</xsl:for-each>
								</xsl:element>
								<xsl:element name="tr">
								<xsl:attribute name="class"><xsl:value-of select="concat('tee-',$TEECOLOUR)"/></xsl:attribute>
									<td class="label">
										Par
									</td>
									<xsl:for-each select="g:tee-set[@colour=$TEECOLOUR]/g:tee">
											<td>
													<b><xsl:value-of select="g:par"/></b>
											</td>
									</xsl:for-each>
								</xsl:element>
								<tr>
									<td class="label">Allowed</td>
									<xsl:for-each select="g:tee-set[@colour=$TEECOLOUR]/g:tee">
											<td>
												<xsl:if test="(number($PHCP) - number(g:handicap-stroke) >= 0)"><b>*</b></xsl:if>
												<xsl:if test="((number($PHCP)-18) - number(g:handicap-stroke) >= 0)"><b>*</b></xsl:if>
												<xsl:if test="((number($PHCP)-36) - number(g:handicap-stroke) >= 0)"><b>*</b></xsl:if>
											</td>
									</xsl:for-each>
								</tr>
								<tr>
									<td class="label">Gross Score</td>
									<xsl:for-each select="g:tee-set[@colour=$TEECOLOUR]/g:tee">
										<td><xsl:text>  </xsl:text></td>
									</xsl:for-each>
								</tr>
								<tr>
									<td class="label">Points</td>
									<xsl:for-each select="g:tee-set[@colour=$TEECOLOUR]/g:tee">
										<td><xsl:text>  </xsl:text></td>
									</xsl:for-each>
								</tr>
								<tr>
									<td class="label">To green</td>
									<xsl:for-each select="g:tee-set[@colour=$TEECOLOUR]/g:tee">
										<td><xsl:text>  </xsl:text></td>
									</xsl:for-each>
								</tr>
								<tr>
									<td class="label">Putts</td>
									<xsl:for-each select="g:tee-set[@colour=$TEECOLOUR]/g:tee">
										<td><xsl:text>  </xsl:text></td>
									</xsl:for-each>
								</tr>
								<tr>
									<td class="label">Penalties</td>
									<xsl:for-each select="g:tee-set[@colour=$TEECOLOUR]/g:tee">
										<td><xsl:text>  </xsl:text></td>
									</xsl:for-each>
								</tr>
							</tbody>
					</table>
			</xsl:if>
        </xsl:template>
</xsl:stylesheet>