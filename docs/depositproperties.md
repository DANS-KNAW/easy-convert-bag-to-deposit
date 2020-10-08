Deposit properties
=================

One of the main differences between an AIP bag and a SIP deposit is the presence of a `deposit.properties` file in the container.
This file contains properties needed for the processing of the deposit.

extra commandline parameter:
```
-s, --source  <arg>  the source of the bag, either VAULT or FEDORA. If FEDORA is used, the --dataverse-identifier-type SHOULD be DOI 
```

The following properties exist

|Property|Format|Description|
|------|--------|---------|
|creation.timestamp	|ISO 8601 datetime, including timezone and in ms precision	|Base this on the 'Created' in bag-info.txt|
|state.label|A state label (see below)|`SUBMITTED`|
|state.description|Text|`This deposit was extracted from the vault and is ready for processing`|  
|depositor.userId|EASY User Account ID|from bag-info.txt|
|deposit.origin|Deposit method.|Use the --source parameter|
|identifier.doi|DOI|take from dataset.xml|
|identifier.fedora|easy-dataset:id in Fedora|take from dataset.xml|
|identifier.urn|URN-NBN|take from dataset.xml| 
|bag-store.bag-id||uuid from bag-directory|
|bag-store.bag-name||name of the bag-dir|
|dataverse.id-identifier|the identifier to be used by Dataverse. This depends on the value of the --dataverse-identifier-type parameter.|The identifier part of either the identifier.urn or identifier.doi|
|dataverse.id-protocol|This depends on the value of the --dataverse-identifier-type parameter|‘urn’ or ‘doi’|
|dataverse.id-authority|This depends on the value of the --dataverse-identifier-type parameter|‘nbn:ui:13’ or the dans-doi prefix|
|dataverse.bag-id|equal to bag-store.bag-id, prefixed with urn:uuid:|urn:uuid:<bag-store.bag-id>
|dataverse.nbn|The URN:NBN of this dataset (version independent) This identifier is used to identify the bag-sequence. Therefor it must contain the urn:nbn of the first version of this bag-sequence. take from dataset.xml of the first version.Alternatively, it can be found in the bag-index (provided it is running v2.0.0 or later)|The urn:nbn of the first version (only equal to identifier.urn in case this is the first version of this dataset)|
|dataverse.other-id|ID used by the depositing organization, may be a PID, must be namespaced. |if identifier.doi is not a DANS-DOI, repeat the value here, otherwise, omit this field
|dataverse.sword-token|The UUID of the first version, the base revision (version independent) this is a new field. It holds the UUID of the first version of this bag-sequence. This can be found in the Is-Version-Of in the bag-info.txt (if it exists) or it is equal to the bag-store.bag-id if this is the first version. Alternatively, it can be found in the bag-index|sword:[uuid of the first version]