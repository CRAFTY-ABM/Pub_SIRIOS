/**
 * 
 */
package org.volante.abm.netsens.util;

import java.util.Set;

import org.apache.log4j.Logger;
import org.rosuda.JRI.REXP;
import org.rosuda.JRI.Rengine;
import org.volante.abm.agent.fr.FunctionalRole;
import org.volante.abm.data.Region;

import de.cesr.more.measures.util.MRService;


/**
 * @author Sascha Holzhauer
 *
 */
public class PatchNumberMeasure {

	/**
	 * Logger
	 */
	static private Logger logger = Logger.getLogger(PatchNumberMeasure.class);
	
	public static double getScore(Region region, Set<FunctionalRole> frs) {
		double sum = 0;
		int counter = 0;
	
		Rengine re = MRService.getRengine();

		REXP result;

		// get raster filename

		logger.info("Calculate AvgPathLength...");
		result = re.eval("");
		logger.info("Result: " + result);
		return result.asDouble();
	}
}
