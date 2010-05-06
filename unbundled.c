//#include <CoreServices/CoreServices.h>

#if __LP64__
typedef unsigned int                    UInt32;
typedef signed int                      SInt32;
#else
typedef unsigned long                   UInt32;
typedef signed long                     SInt32;
#endif

typedef SInt32                          OSStatus;

typedef UInt32 ProcessApplicationTransformState;

struct ProcessSerialNumber {
  UInt32              highLongOfPSN;
  UInt32              lowLongOfPSN;
};
typedef struct ProcessSerialNumber      ProcessSerialNumber;

OSStatus TransformProcessType (
   const ProcessSerialNumber *psn,
   ProcessApplicationTransformState transformState
);

typedef signed short                    SInt16;
typedef SInt16                          OSErr;

OSErr SetFrontProcess (
   const ProcessSerialNumber *PSN
);


#pragma pack(push, 2)

enum {
                                        /* Process identifier - Various reserved process serial numbers */
  kNoProcess                    = 0,
  kSystemProcess                = 1,
  kCurrentProcess               = 2
};

enum {
  kProcessTransformToForegroundApplication = 1
};

void unbundled()
{
    ProcessSerialNumber psn = { 0, kCurrentProcess };
    TransformProcessType( &psn, kProcessTransformToForegroundApplication );
    SetFrontProcess(&psn);
}