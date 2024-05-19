{ **************************************************************************
  FreePascal/Delphi DirectX 12 Header Files
  
  Copyright 2013-2021 Norbert Sonnleitner

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
  ************************************************************************** }

{ **************************************************************************
  Additional Copyright (C) for this modul:

  Copyright (c) Microsoft Corporation.  All rights reserved.

  This unit consists of the following header files
  File name: DXGI.h
             DXGIFormat.h
       DXGIType.h
       DXGICommon.h

  Header version: 10.0.22621.0

  ************************************************************************** }
unit DX12.DXGI;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

{$Z4}

uses
  Windows, Classes, SysUtils;

const
  DXGI_FORMAT_DEFINED = 1;
  DLL_DXGI = 'dxgi.dll';


const
  IID_IDXGIObject: TGUID = '{aec22fb8-76f3-4639-9be0-28eb43a67a2e}';
  IID_IDXGIDeviceSubObject: TGUID = '{3d3e0379-f9de-4d58-bb6c-18d62992f1a6}';
  IID_IDXGIResource: TGUID = '{035f3ab4-482e-4e50-b41f-8a7f8bd8960b}';
  IID_IDXGIKeyedMutex: TGUID = '{9d8e1289-d7b3-465f-8126-250e349af85d}';
  IID_IDXGISurface: TGUID = '{cafcb56c-6ac3-4889-bf47-9e23bbd260ec}';
  IID_IDXGISurface1: TGUID = '{4AE63092-6327-4c1b-80AE-BFE12EA32B86}';
  IID_IDXGIAdapter: TGUID = '{2411e7e1-12ac-4ccf-bd14-9798e8534dc0}';
  IID_IDXGIOutput: TGUID = '{ae02eedb-c735-4690-8d52-5a8dc20213aa}';
  IID_IDXGISwapChain: TGUID = '{310d36a0-d2e7-4c0a-aa04-6a9d23b8886a}';
  IID_IDXGIFactory: TGUID = '{7b7166ec-21c7-44ae-b21a-c9ae321ae369}';
  IID_IDXGIDevice: TGUID = '{54ec77fa-1377-44e6-8c32-88fd5f44c84c}';
  IID_IDXGIFactory1: TGUID = '{770aae78-f26f-4dba-a829-253c83d1b387}';
  IID_IDXGIAdapter1: TGUID = '{29038f61-3839-4626-91fd-086879011a05}';
  IID_IDXGIDevice1: TGUID = '{77db970f-6276-48ba-ba28-070143b4392c}';

const
  DXGI_CPU_ACCESS_NONE = 0;
  DXGI_CPU_ACCESS_DYNAMIC = 1;
  DXGI_CPU_ACCESS_READ_WRITE = 2;
  DXGI_CPU_ACCESS_SCRATCH = 3;
  DXGI_CPU_ACCESS_FIELD = 15;

  DXGI_USAGE_SHADER_INPUT = $00000010;
  DXGI_USAGE_RENDER_TARGET_OUTPUT = $00000020;
  DXGI_USAGE_BACK_BUFFER = $00000040;
  DXGI_USAGE_SHARED = $00000080;
  DXGI_USAGE_READ_ONLY = $00000100;
  DXGI_USAGE_DISCARD_ON_PRESENT = $00000200;
  DXGI_USAGE_UNORDERED_ACCESS = $00000400;

  DXGI_RESOURCE_PRIORITY_MINIMUM = $28000000;
  DXGI_RESOURCE_PRIORITY_LOW = $50000000;
  DXGI_RESOURCE_PRIORITY_NORMAL = $78000000;
  DXGI_RESOURCE_PRIORITY_HIGH = $a0000000;
  DXGI_RESOURCE_PRIORITY_MAXIMUM = $c8000000;

  DXGI_MAP_READ = 1;
  DXGI_MAP_WRITE = 2;
  DXGI_MAP_DISCARD = 4;

  DXGI_ENUM_MODES_INTERLACED = (1);
  DXGI_ENUM_MODES_SCALING = (2);

  DXGI_MAX_SWAP_CHAIN_BUFFERS = 16;

  DXGI_PRESENT_TEST = $00000001;
  DXGI_PRESENT_DO_NOT_SEQUENCE = $00000002;
  DXGI_PRESENT_RESTART = $00000004;
  DXGI_PRESENT_DO_NOT_WAIT = $00000008;
  DXGI_PRESENT_STEREO_PREFER_RIGHT = $00000010;
  DXGI_PRESENT_STEREO_TEMPORARY_MONO = $00000020;
  DXGI_PRESENT_RESTRICT_TO_OUTPUT = $00000040;
  DXGI_PRESENT_USE_DURATION = $00000100;
  DXGI_PRESENT_ALLOW_TEARING = $00000200;

  DXGI_MWA_NO_WINDOW_CHANGES = (1 shl 0);
  DXGI_MWA_NO_ALT_ENTER = (1 shl 1);
  DXGI_MWA_NO_PRINT_SCREEN = (1 shl 2);
  DXGI_MWA_VALID = ($7);

const
  sc_redShift = 16;
  sc_greenShift = 8;
  sc_blueShift = 0;

  sc_redMask: uint32 = $ff shl sc_redShift;
  sc_greenMask: uint32 = $ff shl sc_greenShift;
  sc_blueMask: uint32 = $ff shl sc_blueShift;

type
    {$IFNDEF FPC}
  LONG = longint;
  SIZE_T = ULONG_PTR;
  HMONITOR = THANDLE;
    {$ENDIF}

  TDXGI_FORMAT = (
    DXGI_FORMAT_FROM_FILE=-3,
    DXGI_FORMAT_UNKNOWN = 0,
    DXGI_FORMAT_R32G32B32A32_TYPELESS = 1,
    DXGI_FORMAT_R32G32B32A32_FLOAT = 2,
    DXGI_FORMAT_R32G32B32A32_UINT = 3,
    DXGI_FORMAT_R32G32B32A32_SINT = 4,
    DXGI_FORMAT_R32G32B32_TYPELESS = 5,
    DXGI_FORMAT_R32G32B32_FLOAT = 6,
    DXGI_FORMAT_R32G32B32_UINT = 7,
    DXGI_FORMAT_R32G32B32_SINT = 8,
    DXGI_FORMAT_R16G16B16A16_TYPELESS = 9,
    DXGI_FORMAT_R16G16B16A16_FLOAT = 10,
    DXGI_FORMAT_R16G16B16A16_UNORM = 11,
    DXGI_FORMAT_R16G16B16A16_UINT = 12,
    DXGI_FORMAT_R16G16B16A16_SNORM = 13,
    DXGI_FORMAT_R16G16B16A16_SINT = 14,
    DXGI_FORMAT_R32G32_TYPELESS = 15,
    DXGI_FORMAT_R32G32_FLOAT = 16,
    DXGI_FORMAT_R32G32_UINT = 17,
    DXGI_FORMAT_R32G32_SINT = 18,
    DXGI_FORMAT_R32G8X24_TYPELESS = 19,
    DXGI_FORMAT_D32_FLOAT_S8X24_UINT = 20,
    DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS = 21,
    DXGI_FORMAT_X32_TYPELESS_G8X24_UINT = 22,
    DXGI_FORMAT_R10G10B10A2_TYPELESS = 23,
    DXGI_FORMAT_R10G10B10A2_UNORM = 24,
    DXGI_FORMAT_R10G10B10A2_UINT = 25,
    DXGI_FORMAT_R11G11B10_FLOAT = 26,
    DXGI_FORMAT_R8G8B8A8_TYPELESS = 27,
    DXGI_FORMAT_R8G8B8A8_UNORM = 28,
    DXGI_FORMAT_R8G8B8A8_UNORM_SRGB = 29,
    DXGI_FORMAT_R8G8B8A8_UINT = 30,
    DXGI_FORMAT_R8G8B8A8_SNORM = 31,
    DXGI_FORMAT_R8G8B8A8_SINT = 32,
    DXGI_FORMAT_R16G16_TYPELESS = 33,
    DXGI_FORMAT_R16G16_FLOAT = 34,
    DXGI_FORMAT_R16G16_UNORM = 35,
    DXGI_FORMAT_R16G16_UINT = 36,
    DXGI_FORMAT_R16G16_SNORM = 37,
    DXGI_FORMAT_R16G16_SINT = 38,
    DXGI_FORMAT_R32_TYPELESS = 39,
    DXGI_FORMAT_D32_FLOAT = 40,
    DXGI_FORMAT_R32_FLOAT = 41,
    DXGI_FORMAT_R32_UINT = 42,
    DXGI_FORMAT_R32_SINT = 43,
    DXGI_FORMAT_R24G8_TYPELESS = 44,
    DXGI_FORMAT_D24_UNORM_S8_UINT = 45,
    DXGI_FORMAT_R24_UNORM_X8_TYPELESS = 46,
    DXGI_FORMAT_X24_TYPELESS_G8_UINT = 47,
    DXGI_FORMAT_R8G8_TYPELESS = 48,
    DXGI_FORMAT_R8G8_UNORM = 49,
    DXGI_FORMAT_R8G8_UINT = 50,
    DXGI_FORMAT_R8G8_SNORM = 51,
    DXGI_FORMAT_R8G8_SINT = 52,
    DXGI_FORMAT_R16_TYPELESS = 53,
    DXGI_FORMAT_R16_FLOAT = 54,
    DXGI_FORMAT_D16_UNORM = 55,
    DXGI_FORMAT_R16_UNORM = 56,
    DXGI_FORMAT_R16_UINT = 57,
    DXGI_FORMAT_R16_SNORM = 58,
    DXGI_FORMAT_R16_SINT = 59,
    DXGI_FORMAT_R8_TYPELESS = 60,
    DXGI_FORMAT_R8_UNORM = 61,
    DXGI_FORMAT_R8_UINT = 62,
    DXGI_FORMAT_R8_SNORM = 63,
    DXGI_FORMAT_R8_SINT = 64,
    DXGI_FORMAT_A8_UNORM = 65,
    DXGI_FORMAT_R1_UNORM = 66,
    DXGI_FORMAT_R9G9B9E5_SHAREDEXP = 67,
    DXGI_FORMAT_R8G8_B8G8_UNORM = 68,
    DXGI_FORMAT_G8R8_G8B8_UNORM = 69,
    DXGI_FORMAT_BC1_TYPELESS = 70,
    DXGI_FORMAT_BC1_UNORM = 71,
    DXGI_FORMAT_BC1_UNORM_SRGB = 72,
    DXGI_FORMAT_BC2_TYPELESS = 73,
    DXGI_FORMAT_BC2_UNORM = 74,
    DXGI_FORMAT_BC2_UNORM_SRGB = 75,
    DXGI_FORMAT_BC3_TYPELESS = 76,
    DXGI_FORMAT_BC3_UNORM = 77,
    DXGI_FORMAT_BC3_UNORM_SRGB = 78,
    DXGI_FORMAT_BC4_TYPELESS = 79,
    DXGI_FORMAT_BC4_UNORM = 80,
    DXGI_FORMAT_BC4_SNORM = 81,
    DXGI_FORMAT_BC5_TYPELESS = 82,
    DXGI_FORMAT_BC5_UNORM = 83,
    DXGI_FORMAT_BC5_SNORM = 84,
    DXGI_FORMAT_B5G6R5_UNORM = 85,
    DXGI_FORMAT_B5G5R5A1_UNORM = 86,
    DXGI_FORMAT_B8G8R8A8_UNORM = 87,
    DXGI_FORMAT_B8G8R8X8_UNORM = 88,
    DXGI_FORMAT_R10G10B10_XR_BIAS_A2_UNORM = 89,
    DXGI_FORMAT_B8G8R8A8_TYPELESS = 90,
    DXGI_FORMAT_B8G8R8A8_UNORM_SRGB = 91,
    DXGI_FORMAT_B8G8R8X8_TYPELESS = 92,
    DXGI_FORMAT_B8G8R8X8_UNORM_SRGB = 93,
    DXGI_FORMAT_BC6H_TYPELESS = 94,
    DXGI_FORMAT_BC6H_UF16 = 95,
    DXGI_FORMAT_BC6H_SF16 = 96,
    DXGI_FORMAT_BC7_TYPELESS = 97,
    DXGI_FORMAT_BC7_UNORM = 98,
    DXGI_FORMAT_BC7_UNORM_SRGB = 99,
    DXGI_FORMAT_AYUV = 100,
    DXGI_FORMAT_Y410 = 101,
    DXGI_FORMAT_Y416 = 102,
    DXGI_FORMAT_NV12 = 103,
    DXGI_FORMAT_P010 = 104,
    DXGI_FORMAT_P016 = 105,
    DXGI_FORMAT_420_OPAQUE = 106,
    DXGI_FORMAT_YUY2 = 107,
    DXGI_FORMAT_Y210 = 108,
    DXGI_FORMAT_Y216 = 109,
    DXGI_FORMAT_NV11 = 110,
    DXGI_FORMAT_AI44 = 111,
    DXGI_FORMAT_IA44 = 112,
    DXGI_FORMAT_P8 = 113,
    DXGI_FORMAT_A8P8 = 114,
    DXGI_FORMAT_B4G4R4A4_UNORM = 115,

    DXGI_FORMAT_R10G10B10_7E3_A2_FLOAT = 116,
    DXGI_FORMAT_R10G10B10_6E4_A2_FLOAT  = 117,

    DXGI_FORMAT_P208 = 130,
    DXGI_FORMAT_V208 = 131,
    DXGI_FORMAT_V408 = 132,
    DXGI_FORMAT_SAMPLER_FEEDBACK_MIN_MIP_OPAQUE = 189,
    DXGI_FORMAT_SAMPLER_FEEDBACK_MIP_REGION_USED_OPAQUE = 190,

    DXGI_FORMAT_FORCE_UINT = INT32($ffffffff));

  PDXGI_FORMAT = ^TDXGI_FORMAT;

const
  // DXGI error messages have moved to winerror.h
  _FACDXGI = $87A;
  MAKE_DXGI_HRESULT = longint(_FACDXGI shl 16) or longint(1 shl 31);
  MAKE_DXGI_STATUS = longint(_FACDXGI shl 16);

  DXGI_STATUS_OCCLUDED = MAKE_DXGI_STATUS or 1;
  DXGI_STATUS_CLIPPED = MAKE_DXGI_STATUS or 2;
  DXGI_STATUS_NO_REDIRECTION = MAKE_DXGI_STATUS or 4;
  DXGI_STATUS_NO_DESKTOP_ACCESS = MAKE_DXGI_STATUS or 5;
  DXGI_STATUS_GRAPHICS_VIDPN_SOURCE_IN_USE = MAKE_DXGI_STATUS or 6;
  DXGI_STATUS_MODE_CHANGED = MAKE_DXGI_STATUS or 7;
  DXGI_STATUS_MODE_CHANGE_IN_PROGRESS = MAKE_DXGI_STATUS or 8;


  DXGI_ERROR_INVALID_CALL = MAKE_DXGI_HRESULT or 1;
  DXGI_ERROR_NOT_FOUND = MAKE_DXGI_HRESULT or 2;
  DXGI_ERROR_MORE_DATA = MAKE_DXGI_HRESULT or 3;
  DXGI_ERROR_UNSUPPORTED = MAKE_DXGI_HRESULT or 4;
  DXGI_ERROR_DEVICE_REMOVED = MAKE_DXGI_HRESULT or 5;
  DXGI_ERROR_DEVICE_HUNG = MAKE_DXGI_HRESULT or 6;
  DXGI_ERROR_DEVICE_RESET = MAKE_DXGI_HRESULT or 7;
  DXGI_ERROR_WAS_STILL_DRAWING = MAKE_DXGI_HRESULT or 10;
  DXGI_ERROR_FRAME_STATISTICS_DISJOINT = MAKE_DXGI_HRESULT or 11;
  DXGI_ERROR_GRAPHICS_VIDPN_SOURCE_IN_USE = MAKE_DXGI_HRESULT or 12;
  DXGI_ERROR_DRIVER_INTERNAL_ERROR = MAKE_DXGI_HRESULT or 32;
  DXGI_ERROR_NONEXCLUSIVE = MAKE_DXGI_HRESULT or 33;
  DXGI_ERROR_NOT_CURRENTLY_AVAILABLE = MAKE_DXGI_HRESULT or 34;
  DXGI_ERROR_REMOTE_CLIENT_DISCONNECTED = MAKE_DXGI_HRESULT or 35;
  DXGI_ERROR_REMOTE_OUTOFMEMORY = MAKE_DXGI_HRESULT or 36;


  // The following values are used with DXGI_SAMPLE_DESC::Quality:
  DXGI_STANDARD_MULTISAMPLE_QUALITY_PATTERN = $ffffffff;
  DXGI_CENTER_MULTISAMPLE_QUALITY_PATTERN = $fffffffe;

type

  PIUnknown = ^IUnknown; // ????

  TDXGI_RGB = record
    Red: single;
    Green: single;
    Blue: single;
  end;

  { TD3DCOLORVALUE }

  TD3DCOLORVALUE = record
    procedure Init(rgb: uint32; alpha: single); overload;
    procedure Init(lr, lg, lb, la: single); overload;
    case integer of
      0: (
        r: single;
        g: single;
        b: single;
        a: single;);
      1: (dvR: single;
        dvG: single;
        dvB: single;
        dvA: single;);


  end;

  TDXGI_RGBA = TD3DCOLORVALUE;
  PDXGI_RGBA = ^TDXGI_RGBA;

  TDXGI_GAMMA_CONTROL = record
    Scale: TDXGI_RGB;
    Offset: TDXGI_RGB;
    GammaCurve: array [0.. 1024] of TDXGI_RGB;
  end;

  PDXGI_GAMMA_CONTROL = ^TDXGI_GAMMA_CONTROL;

  TDXGI_GAMMA_CONTROL_CAPABILITIES = record
    ScaleAndOffsetSupported: longbool;
    MaxConvertedValue: single;
    MinConvertedValue: single;
    NumGammaControlPoints: UINT;
    ControlPointPositions: array [0..1024] of single;
  end;

  PDXGI_GAMMA_CONTROL_CAPABILITIES = ^TDXGI_GAMMA_CONTROL_CAPABILITIES;

  TDXGI_RATIONAL = record
    Numerator: UINT;
    Denominator: UINT;
  end;

  PDXGI_RATIONAL = ^TDXGI_RATIONAL;


  TDXGI_MODE_SCANLINE_ORDER = (
    DXGI_MODE_SCANLINE_ORDER_UNSPECIFIED = 0,
    DXGI_MODE_SCANLINE_ORDER_PROGRESSIVE = 1,
    DXGI_MODE_SCANLINE_ORDER_UPPER_FIELD_FIRST = 2,
    DXGI_MODE_SCANLINE_ORDER_LOWER_FIELD_FIRST = 3);

  TDXGI_MODE_SCALING = (
    DXGI_MODE_SCALING_UNSPECIFIED = 0,
    DXGI_MODE_SCALING_CENTERED = 1,
    DXGI_MODE_SCALING_STRETCHED = 2);

  TDXGI_MODE_ROTATION = (
    DXGI_MODE_ROTATION_UNSPECIFIED = 0,
    DXGI_MODE_ROTATION_IDENTITY = 1,
    DXGI_MODE_ROTATION_ROTATE90 = 2,
    DXGI_MODE_ROTATION_ROTATE180 = 3,
    DXGI_MODE_ROTATION_ROTATE270 = 4);

  { TDXGI_MODE_DESC }

  TDXGI_MODE_DESC = record
    Width: UINT;
    Height: UINT;
    RefreshRate: TDXGI_RATIONAL;
    Format: TDXGI_FORMAT;
    ScanlineOrdering: TDXGI_MODE_SCANLINE_ORDER;
    Scaling: TDXGI_MODE_SCALING;
        {$IFDEF FPC}
        class operator Initialize(var A: TDXGI_MODE_DESC);
        {$ENDIF}
    procedure Init;
  end;

  PDXGI_MODE_DESC = ^TDXGI_MODE_DESC;

  { TDXGI_SAMPLE_DESC }

  TDXGI_SAMPLE_DESC = record
    Count: UINT;
    Quality: UINT;
        {$IFDEF FPC}
        class operator Initialize(var A: TDXGI_SAMPLE_DESC);
        {$ENDIF}
    procedure Init;
  end;

  PDXGI_SAMPLE_DESC = ^TDXGI_SAMPLE_DESC;


  TDXGI_COLOR_SPACE_TYPE = (
    DXGI_COLOR_SPACE_RGB_FULL_G22_NONE_P709 = 0,
    DXGI_COLOR_SPACE_RGB_FULL_G10_NONE_P709 = 1,
    DXGI_COLOR_SPACE_RGB_STUDIO_G22_NONE_P709 = 2,
    DXGI_COLOR_SPACE_RGB_STUDIO_G22_NONE_P2020 = 3,
    DXGI_COLOR_SPACE_RESERVED = 4,
    DXGI_COLOR_SPACE_YCBCR_FULL_G22_NONE_P709_X601 = 5,
    DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P601 = 6,
    DXGI_COLOR_SPACE_YCBCR_FULL_G22_LEFT_P601 = 7,
    DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P709 = 8,
    DXGI_COLOR_SPACE_YCBCR_FULL_G22_LEFT_P709 = 9,
    DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_LEFT_P2020 = 10,
    DXGI_COLOR_SPACE_YCBCR_FULL_G22_LEFT_P2020 = 11,
    DXGI_COLOR_SPACE_RGB_FULL_G2084_NONE_P2020 = 12,
    DXGI_COLOR_SPACE_YCBCR_STUDIO_G2084_LEFT_P2020 = 13,
    DXGI_COLOR_SPACE_RGB_STUDIO_G2084_NONE_P2020 = 14,
    DXGI_COLOR_SPACE_YCBCR_STUDIO_G22_TOPLEFT_P2020 = 15,
    DXGI_COLOR_SPACE_YCBCR_STUDIO_G2084_TOPLEFT_P2020 = 16,
    DXGI_COLOR_SPACE_RGB_FULL_G22_NONE_P2020 = 17,
    DXGI_COLOR_SPACE_YCBCR_STUDIO_GHLG_TOPLEFT_P2020 = 18,
    DXGI_COLOR_SPACE_YCBCR_FULL_GHLG_TOPLEFT_P2020 = 19,
    DXGI_COLOR_SPACE_RGB_STUDIO_G24_NONE_P709 = 20,
    DXGI_COLOR_SPACE_RGB_STUDIO_G24_NONE_P2020 = 21,
    DXGI_COLOR_SPACE_YCBCR_STUDIO_G24_LEFT_P709 = 22,
    DXGI_COLOR_SPACE_YCBCR_STUDIO_G24_LEFT_P2020 = 23,
    DXGI_COLOR_SPACE_YCBCR_STUDIO_G24_TOPLEFT_P2020 = 24,
    DXGI_COLOR_SPACE_CUSTOM = INT32($FFFFFFFF));

  TDXGI_JPEG_DC_HUFFMAN_TABLE = record
    CodeCounts: array [0..11] of byte;
    CodeValues: array [0..11] of byte;
  end;

  TDXGI_JPEG_AC_HUFFMAN_TABLE = record
    CodeCounts: array [0..15] of byte;
    CodeValues: array [0..161] of byte;
  end;

  TDXGI_JPEG_QUANTIZATION_TABLE = record
    Elements: array [0..63] of byte;
  end;


type
  TDXGI_USAGE = UINT;

  TDXGI_FRAME_STATISTICS = record
    PresentCount: UINT;
    PresentRefreshCount: UINT;
    SyncRefreshCount: UINT;
    SyncQPCTime: LARGE_INTEGER;
    SyncGPUTime: LARGE_INTEGER;
  end;


  TDXGI_MAPPED_RECT = record
    Pitch: integer;
    pBits: pbyte;
  end;


  TLUID = record
    LowPart: DWORD;
    HighPart: LONG;
  end;

  PLUID = ^TLUID;


  TDXGI_ADAPTER_DESC = record
    Description: array [0.. 127] of widechar;
    VendorId: UINT;
    DeviceId: UINT;
    SubSysId: UINT;
    Revision: UINT;
    DedicatedVideoMemory: SIZE_T;
    DedicatedSystemMemory: SIZE_T;
    SharedSystemMemory: SIZE_T;
    AdapterLuid: TLUID;
  end;


  //HMONITOR = HANDLE;


  TDXGI_OUTPUT_DESC = record
    DeviceName: array [0.. 31] of widechar;
    DesktopCoordinates: TRECT;
    AttachedToDesktop: longbool;
    Rotation: TDXGI_MODE_ROTATION;
    Monitor: HMONITOR;
  end;

  PDXGI_OUTPUT_DESC = ^TDXGI_OUTPUT_DESC;

  TDXGI_SHARED_RESOURCE = record
    Handle: THANDLE;
  end;
  PDXGI_SHARED_RESOURCE = ^TDXGI_SHARED_RESOURCE;

  TDXGI_RESIDENCY = (
    DXGI_RESIDENCY_FULLY_RESIDENT = 1,
    DXGI_RESIDENCY_RESIDENT_IN_SHARED_MEMORY = 2,
    DXGI_RESIDENCY_EVICTED_TO_DISK = 3
    );

  PDXGI_RESIDENCY = ^TDXGI_RESIDENCY;

  TDXGI_SURFACE_DESC = record
    Width: UINT;
    Height: UINT;
    Format: TDXGI_FORMAT;
    SampleDesc: TDXGI_SAMPLE_DESC;
  end;

  PDXGI_SURFACE_DESC = ^TDXGI_SURFACE_DESC;

  TDXGI_SWAP_EFFECT = (
    DXGI_SWAP_EFFECT_DISCARD = 0,
    DXGI_SWAP_EFFECT_SEQUENTIAL = 1,
    DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL = 3,
    DXGI_SWAP_EFFECT_FLIP_DISCARD = 4
    );

  TDXGI_SWAP_CHAIN_FLAG = (
    DXGI_SWAP_CHAIN_FLAG_NONPREROTATED = 1,
    DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH = 2,
    DXGI_SWAP_CHAIN_FLAG_GDI_COMPATIBLE = 4,
    DXGI_SWAP_CHAIN_FLAG_RESTRICTED_CONTENT = 8,
    DXGI_SWAP_CHAIN_FLAG_RESTRICT_SHARED_RESOURCE_DRIVER = 16,
    DXGI_SWAP_CHAIN_FLAG_DISPLAY_ONLY = 32,
    DXGI_SWAP_CHAIN_FLAG_FRAME_LATENCY_WAITABLE_OBJECT = 64,
    DXGI_SWAP_CHAIN_FLAG_FOREGROUND_LAYER = 128,
    DXGI_SWAP_CHAIN_FLAG_FULLSCREEN_VIDEO = 256,
    DXGI_SWAP_CHAIN_FLAG_YUV_VIDEO = 512,
    DXGI_SWAP_CHAIN_FLAG_HW_PROTECTED = 1024,
    DXGI_SWAP_CHAIN_FLAG_ALLOW_TEARING = 2048,
    DXGI_SWAP_CHAIN_FLAG_RESTRICTED_TO_ALL_HOLOGRAPHIC_DISPLAYS = 4096
    );

  { TDXGI_SWAP_CHAIN_DESC }

  TDXGI_SWAP_CHAIN_DESC = record
    BufferDesc: TDXGI_MODE_DESC;
    SampleDesc: TDXGI_SAMPLE_DESC;
    BufferUsage: TDXGI_USAGE;
    BufferCount: UINT;
    OutputWindow: HWND;
    Windowed: longbool;
    SwapEffect: TDXGI_SWAP_EFFECT;
    Flags: UINT;
        {$IFDEF FPC}
        class operator Initialize(var A: TDXGI_SWAP_CHAIN_DESC);
        {$ENDIF}
    procedure Init;
  end;

  PDXGI_SWAP_CHAIN_DESC = ^TDXGI_SWAP_CHAIN_DESC;


  IDXGIObject = interface(IUnknown)
    ['{aec22fb8-76f3-4639-9be0-28eb43a67a2e}']
    function SetPrivateData(Name: TGUID; DataSize: UINT; const pData{DataSize}: Pointer): HResult; stdcall;
    function SetPrivateDataInterface(Name: TGUID; const pUnknown: IUnknown): HResult; stdcall;
    function GetPrivateData(Name: TGUID; var pDataSize: UINT; out pData{pDataSize}: Pointer): HResult; stdcall;
    function GetParent(const riid: TGUID; out ppParent: Pointer): HResult; stdcall;
  end;

  IDXGIDeviceSubObject = interface(IDXGIObject)
    ['{3d3e0379-f9de-4d58-bb6c-18d62992f1a6}']
    function GetDevice(const riid: TGUID; out ppDevice: pointer): HResult; stdcall;
  end;

  IDXGIResource = interface(IDXGIDeviceSubObject)
    ['{035f3ab4-482e-4e50-b41f-8a7f8bd8960b}']
    function GetSharedHandle(out pSharedHandle: THANDLE): HResult; stdcall;
    function GetUsage(out pUsage: TDXGI_USAGE): HResult; stdcall;
    function SetEvictionPriority(EvictionPriority: UINT): HResult; stdcall;
    function GetEvictionPriority(out pEvictionPriority: UINT): HResult; stdcall;
  end;

  PIDXGIResource = ^IDXGIResource;

  IDXGIKeyedMutex = interface(IDXGIDeviceSubObject)
    ['{9d8e1289-d7b3-465f-8126-250e349af85d}']
    function AcquireSync(Key: uint64; dwMilliseconds: DWORD): HResult; stdcall;
    function ReleaseSync(Key: uint64): HResult; stdcall;
  end;

  IDXGISurface = interface(IDXGIDeviceSubObject)
    ['{cafcb56c-6ac3-4889-bf47-9e23bbd260ec}']
    function GetDesc(out pDesc: TDXGI_SURFACE_DESC): HResult; stdcall;
    function Map(out pLockedRect: TDXGI_MAPPED_RECT; MapFlags: UINT): HResult; stdcall;
    function Unmap(): HResult; stdcall;
  end;

  PIDXGISurface = ^IDXGISurface;


  IDXGISurface1 = interface(IDXGISurface)
    ['{4AE63092-6327-4c1b-80AE-BFE12EA32B86}']
    function GetDC(Discard: longbool; out phdc: HDC): HResult; stdcall;
    function ReleaseDC(pDirtyRect: PRECT): HResult; stdcall;
  end;

  IDXGIOutput = interface;

  IDXGIAdapter = interface(IDXGIObject)
    ['{2411e7e1-12ac-4ccf-bd14-9798e8534dc0}']
    function EnumOutputs(Output: UINT; out ppOutput: IDXGIOutput): HResult; stdcall;
    function GetDesc(out pDesc: TDXGI_ADAPTER_DESC): HResult; stdcall;
    function CheckInterfaceSupport(const InterfaceName: TGUID; out pUMDVersion: LARGE_INTEGER): HResult; stdcall;
  end;

  PIDXGIOutput = ^IDXGIOutput;

  IDXGIOutput = interface(IDXGIObject)
    ['{ae02eedb-c735-4690-8d52-5a8dc20213aa}']
    function GetDesc(out pDesc: TDXGI_OUTPUT_DESC): HResult; stdcall;
    function GetDisplayModeList(EnumFormat: TDXGI_FORMAT; Flags: UINT; var pNumModes: UINT;
    { out} pDesc: PDXGI_MODE_DESC): HResult; stdcall;
    function FindClosestMatchingMode(const pModeToMatch: PDXGI_MODE_DESC; out pClosestMatch: TDXGI_MODE_DESC;
      pConcernedDevice: IUnknown): HResult; stdcall;
    function WaitForVBlank(): HResult; stdcall;
    function TakeOwnership(pDevice: IUnknown; Exclusive: longbool): HResult; stdcall;
    procedure ReleaseOwnership(); stdcall;
    function GetGammaControlCapabilities(out pGammaCaps: TDXGI_GAMMA_CONTROL_CAPABILITIES): HResult; stdcall;
    function SetGammaControl(const pArray: PDXGI_GAMMA_CONTROL): HResult; stdcall;
    function GetGammaControl(out pArray: TDXGI_GAMMA_CONTROL): HResult; stdcall;
    function SetDisplaySurface(pScanoutSurface: IDXGISurface): HResult; stdcall;
    function GetDisplaySurfaceData(pDestination: IDXGISurface): HResult; stdcall;
    function GetFrameStatistics(out pStats: TDXGI_FRAME_STATISTICS): HResult; stdcall;
  end;

  IDXGISwapChain = interface(IDXGIDeviceSubObject)
    ['{310d36a0-d2e7-4c0a-aa04-6a9d23b8886a}']
    function Present(SyncInterval: UINT; Flags: UINT): HResult; stdcall;
    function GetBuffer(Buffer: UINT; const riid: TGUID; out ppSurface): HResult; stdcall;
    function SetFullscreenState(Fullscreen: longbool; pTarget: IDXGIOutput): HResult; stdcall;
    function GetFullscreenState(out pFullscreen: longbool; {out} ppTarget: PIDXGIOutput): HResult; stdcall;
    function GetDesc(out pDesc: TDXGI_SWAP_CHAIN_DESC): HResult; stdcall;
    function ResizeBuffers(BufferCount: UINT; Width: UINT; Height: UINT; NewFormat: TDXGI_FORMAT;
      SwapChainFlags: UINT): HResult; stdcall;
    function ResizeTarget(const pNewTargetParameters: PDXGI_MODE_DESC): HResult; stdcall;
    function GetContainingOutput(out ppOutput: IDXGIOutput): HResult; stdcall;
    function GetFrameStatistics(out pStats: TDXGI_FRAME_STATISTICS): HResult; stdcall;
    function GetLastPresentCount(out pLastPresentCount: UINT): HResult; stdcall;
  end;

  IDXGIFactory = interface(IDXGIObject)
    ['{7b7166ec-21c7-44ae-b21a-c9ae321ae369}']
    function EnumAdapters(Adapter: UINT; out ppAdapter: IDXGIAdapter): HResult; stdcall;
    function MakeWindowAssociation(WindowHandle: HWND; Flags: UINT): HResult; stdcall;
    function GetWindowAssociation(out pWindowHandle: HWND): HResult; stdcall;
    function CreateSwapChain(pDevice: IUnknown; pDesc: PDXGI_SWAP_CHAIN_DESC; out ppSwapChain: IDXGISwapChain): HResult; stdcall;
    function CreateSoftwareAdapter(Module: HMODULE; out ppAdapter: IDXGIAdapter): HResult; stdcall;
  end;

  PIDXGIFactory = ^IDXGIFactory;


  IDXGIDevice = interface(IDXGIObject)
    ['{54ec77fa-1377-44e6-8c32-88fd5f44c84c}']
    function GetAdapter(out pAdapter: IDXGIAdapter): HResult; stdcall;
    function CreateSurface(const pDesc: PDXGI_SURFACE_DESC; NumSurfaces: UINT; Usage: TDXGI_USAGE;
      const pSharedResource: PDXGI_SHARED_RESOURCE; out {NumSurfaces} ppSurface: PIDXGISurface): HResult; stdcall;
    function QueryResourceResidency(const ppResources: PIUnknown; out pResidencyStatus: PDXGI_RESIDENCY;
      NumResources: UINT): HResult; stdcall;
    function SetGPUThreadPriority(Priority: integer): HResult; stdcall;
    function GetGPUThreadPriority(out pPriority: integer): HResult; stdcall;
  end;


  TDXGI_ADAPTER_FLAG = (
    DXGI_ADAPTER_FLAG_NONE = 0,
    DXGI_ADAPTER_FLAG_REMOTE = 1,
    DXGI_ADAPTER_FLAG_SOFTWARE = 2,
    DXGI_ADAPTER_FLAG_FORCE_DWORD = INT32($ffffffff)
    );

  TDXGI_ADAPTER_DESC1 = record
    Description: array [0.. 127] of widechar;
    VendorId: UINT;
    DeviceId: UINT;
    SubSysId: UINT;
    Revision: UINT;
    DedicatedVideoMemory: SIZE_T;
    DedicatedSystemMemory: SIZE_T;
    SharedSystemMemory: SIZE_T;
    AdapterLuid: TLUID;
    Flags: UINT;
  end;

  TDXGI_DISPLAY_COLOR_SPACE = record
    PrimaryCoordinates: array [0..7, 0..1] of single;
    WhitePoints: array [0.. 15, 0..1] of single;
  end;

  IDXGIAdapter1 = interface;

  IDXGIFactory1 = interface(IDXGIFactory)
    ['{770aae78-f26f-4dba-a829-253c83d1b387}']
    function EnumAdapters1(Adapter: UINT; out ppAdapter: IDXGIAdapter1): HResult; stdcall;
    function IsCurrent(): longbool; stdcall;
  end;

  IDXGIAdapter1 = interface(IDXGIAdapter)
    ['{29038f61-3839-4626-91fd-086879011a05}']
    function GetDesc1(out pDesc: TDXGI_ADAPTER_DESC1): HResult; stdcall;
  end;

  IDXGIDevice1 = interface(IDXGIDevice)
    ['{77db970f-6276-48ba-ba28-070143b4392c}']
    function SetMaximumFrameLatency(MaxLatency: UINT): HResult; stdcall;
    function GetMaximumFrameLatency(out pMaxLatency: UINT): HResult; stdcall;
  end;

function CreateDXGIFactory(const riid: TGUID; out ppFactory): HResult; stdcall; external DLL_DXGI;
function CreateDXGIFactory1(const riid: TGUID; out ppFactory): HResult; stdcall; external DLL_DXGI;

implementation

{ TDXGI_SWAP_CHAIN_DESC }

{$IFDEF FPC}
class operator TDXGI_SWAP_CHAIN_DESC.Initialize(var A: TDXGI_SWAP_CHAIN_DESC);
begin
   // a.BufferDesc: TDXGI_MODE_DESC;
   // a.SampleDesc: TDXGI_SAMPLE_DESC;
    a.BufferUsage:=DXGI_USAGE_RENDER_TARGET_OUTPUT;
    a.BufferCount:=0;
    a.OutputWindow:=0;
    a.Windowed:=False;
    a.SwapEffect:=DXGI_SWAP_EFFECT_DISCARD;
    a.Flags:=0;
end;
{$ENDIF}

procedure TDXGI_SWAP_CHAIN_DESC.Init;
begin
  BufferUsage := DXGI_USAGE_RENDER_TARGET_OUTPUT;
  BufferCount := 0;
  OutputWindow := 0;
  Windowed := False;
  SwapEffect := DXGI_SWAP_EFFECT_DISCARD;
  Flags := 0;
end;


{ TDXGI_MODE_DESC }

 {$IFDEF FPC}
class operator TDXGI_MODE_DESC.Initialize(var A: TDXGI_MODE_DESC);
begin
    a.Width:=0;
    a.Height:=0;
    a.RefreshRate.Numerator:=0;
    a.RefreshRate.Denominator:=1;
    a.Format:=DXGI_FORMAT_UNKNOWN;
    a.ScanlineOrdering:=DXGI_MODE_SCANLINE_ORDER_UNSPECIFIED;
    a.Scaling:=DXGI_MODE_SCALING_UNSPECIFIED;
end;
{$ENDIF}

procedure TDXGI_MODE_DESC.Init;
begin
  Width := 0;
  Height := 0;
  RefreshRate.Numerator := 0;
  RefreshRate.Denominator := 1;
  Format := DXGI_FORMAT_UNKNOWN;
  ScanlineOrdering := DXGI_MODE_SCANLINE_ORDER_UNSPECIFIED;
  Scaling := DXGI_MODE_SCALING_UNSPECIFIED;
end;

{ TDXGI_SAMPLE_DESC }

{$IFDEF FPC}
class operator TDXGI_SAMPLE_DESC.Initialize(var A: TDXGI_SAMPLE_DESC);
begin
   // Default values MSDN
   a.Count:=1;
   a.Quality:=0;
end;
{$ENDIF}

procedure TDXGI_SAMPLE_DESC.Init;
begin
  // Default values MSDN
  Count := 1;
  Quality := 0;
end;


{ TD3DCOLORVALUE }

procedure TD3DCOLORVALUE.Init(rgb: uint32; alpha: single);
begin
  r := ((rgb and sc_redMask) shr sc_redShift) / 255.0;
  g := ((rgb and sc_greenMask) shr sc_greenShift) / 255.0;
  b := ((rgb and sc_blueMask) shr sc_blueShift) / 255.0;
  a := alpha;
end;



procedure TD3DCOLORVALUE.Init(lr, lg, lb, la: single);
begin
  r := lr;
  g := lg;
  b := lb;
  a := la;
end;

end.
