/**
 * This file is part of
 * 
 * CRAFTY - Competition for Resources between Agent Functional TYpes
 *
 * Copyright (C) 2014 School of GeoScience, University of Edinburgh, Edinburgh, UK
 * 
 * CRAFTY is free software: You can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software 
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 *  
 * CRAFTY is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty
 * of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * School of Geoscience, University of Edinburgh, Edinburgh, UK
 */
package org.volante.abm.netsens.output;


import java.util.HashSet;
import java.util.Set;

import org.volante.abm.agent.fr.FunctionalRole;
import org.volante.abm.data.ModelData;
import org.volante.abm.data.Region;
import org.volante.abm.data.Regions;
import org.volante.abm.example.measures.ConnectivityMeasure;
import org.volante.abm.output.AggregateCSVOutputter;
import org.volante.abm.output.TableColumn;
import org.volante.abm.schedule.RunInfo;


/**
 * 
 * @author Sascha Holzhauer
 * 
 */
public class FRConnectivityCSVOutputter extends AggregateCSVOutputter {

	boolean initialised = false;

	/**
	 * @see org.volante.abm.output.AbstractOutputter#getDefaultOutputName()
	 */
	@Override
	public String getDefaultOutputName() {
		return "LandUseConnectivity";
	}

	/**
	 * @see org.volante.abm.serialization.GloballyInitialisable#initialise(org.volante.abm.data.ModelData,
	 *      org.volante.abm.schedule.RunInfo, org.volante.abm.data.Regions)
	 */
	@Override
	public void doOutput(Regions regions) {
		if (!initialised) {
			Set<FunctionalRole> frs = new HashSet<>();
			Set<String> frLabels = new HashSet<>();

			for (Region r : regions.getAllRegions()) {
				for (FunctionalRole fr : r.getFunctionalRoles()) {
					if (!frLabels.contains(fr.getLabel())) {
						frs.add(fr);
						frLabels.add(fr.getLabel());
					}
				}
			}
			for (FunctionalRole fr : frs) {
				addColumn(new ConnectivityColumn(fr));
			}
			this.initialised = true;
		}
		super.doOutput(regions);
	}

	public class ConnectivityColumn implements TableColumn<Region> {
		FunctionalRole fr;

		public ConnectivityColumn(FunctionalRole fr) {
			this.fr = fr;
		}

		@Override
		public String getHeader() {
			return fr.getLabel();
		}

		@Override
		public String getValue(Region r, ModelData data, RunInfo info, Regions rs) {
			return doubleFmt.format(ConnectivityMeasure.getScore(r, this.fr));
		}
	}
}