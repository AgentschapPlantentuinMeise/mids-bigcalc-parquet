# MIDS calculation based on GBIF snapshots

This repository includes code to calculate MIDS levels and information element achievements for snapshot data of GBIF. More info on the MIDS standard can be found on the [TDWG MIDS standard draft website](https://mids.tdwg.org/). This work is based on previous efforts to calculate MIDS at the full GBIF scale ([Dillen et al. 2025](https://doi.org/10.3897/biss.9.181898)), which used a snapshot of GBIF with Preserved Specimen records only, retrieved through the SQL API, and applied the [MIDSCalculator code](https://github.com/AgentschapPlantentuinMeise/MIDSCalculator) in batch on this large dataset (~120 GB).

The code in this repository operates on parquet snapshots instead, which offers significantly more efficient computational and I/O efficiency. GBIF has been offering monthly full parquet snapshots for several years now, but these only include selected properties, and some key ones for MIDS are missing (e.g. dwc:preparations). Because of these absent properties, we can only resort to full Darwin Core Archive snapshots as our historical data source. Such snapshots are available going back to at least 2016, though they have become increasingly unwieldy.

Snapshots with only preserved specimen data also exist, but few data files have been retained by GBIF the further back in time we go, as many GBIF download payloads fail to get cited. Hence, the workflow has been using parquet files derived from full GBIF DwC-A snapshots (including billions of observed birds). These massive files were downloaded to a machine with sufficient memory, where they were decompressed and the contents transformed into more wieldy parquet files. The workflow in this repository uses those parquet files.

## Structure of the repo

The main script is `parq.R`, which streams the parquet file using `arrow` and processes the file to calculate the MIDS level for each record, as well as retaining the binary map for each information element. It also indexes some properties like country and year for downstream analysis. The calculation re-uses functions from the MIDSCalculator tool, which needs to be available on the same machine (its local path should be specified in `parq.R`). The result is written as a parquet file in the `outputs` directory.

The path to the parquet file can be specified in `config.ini`, which is otherwise a copy of the config used for the MIDSCalculator scripts. The SSSOM mappings (under `sssom`) used for the MIDS calculation are also incorporated into this repository, should they need customization, for instance to omit properties still missing (e.g. those mapped to mids:MediaID). Currently they are identical to the dwc-a mappings for biology in the MIDSCalculator tool (and the official MIDS repository, though they are not normative).

The `analyze_results.R` script derives some summary statistics from the generated .parquet file and plots them into a barchart.
