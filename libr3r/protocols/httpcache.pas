unit HttpCache;

interface

uses
	Headers;

type
	TCacheType = (ctEtag, ctExpires, ctLastModified);

	TCacheInfo = record
		CacheType: TCacheType;
		DateInfo: TDateTime;
		FeedData: String;
		Headers: THeaders;
	end;

implementation

end.
