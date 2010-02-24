<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
				xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
				xmlns:xs="http://www.w3.org/2001/XMLSchema"
				xmlns:g="http://code.google.com/p/golfml"
				xmlns:xlink="http://www.w3.org/1999/xlink"
>
<!-- convgps.xsl
	
	DESCRIPTION
	
	Convert position-gps element into simpler gps element with attributes.

	 From
	 <position-gps><lat>0.1234</lat><lon>0.5678</lon><alt mode="absolute">12</alt></position-gps>

	 To
	   <gps lat="0.1234" lon="0.5678" alt="12" alt-mode="absolute"/>
	

    VERSION
	  $Revision$
	
	HISTORY
	  Feb 2010: Created.
	
-->

	<xd:doc xmlns:xd="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet">
		<xd:desc>
			<xd:p><xd:b>Created on:</xd:b> Feb 18, 2010</xd:p>
			<xd:p><xd:b>Author:</xd:b> pierre</xd:p>
			<xd:p>Convert gps-position element into simpler gps element with attributes.</xd:p>
		</xd:desc>
	</xd:doc>
	
	
	<xsl:template match="g:position-gps">
		<xsl:element name="gps" namespace="http://code.google.com/p/golfml">
			<xsl:attribute name="lat"><xsl:value-of select="g:lat"/></xsl:attribute>
			<xsl:attribute name="lon"><xsl:value-of select="g:lon"/></xsl:attribute>
			<xsl:if test="g:alt">
				<xsl:attribute name="alt"><xsl:value-of select="g:alt"/></xsl:attribute>
				<xsl:if test="g:alt/@mode">
					<xsl:attribute name="alt-mode"><xsl:value-of select="g:alt/@mode"/></xsl:attribute>
				</xsl:if>
			</xsl:if>
		</xsl:element>
	</xsl:template>
	
	<xsl:template match="@*|node()">
		<xsl:copy>
			<xsl:apply-templates select="@*|node()"/>
		</xsl:copy>
	</xsl:template>
</xsl:stylesheet>
