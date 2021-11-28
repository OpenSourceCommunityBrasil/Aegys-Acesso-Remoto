{**************************************************************************************}
{                                                                                      }
{ CCR.Clipboard - resource strings                                                     }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 2.0      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at https://www.mozilla.org/MPL/2.0              }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2012-14 Chris Rolliston. All Rights Reserved.      }
{                                                                                      }
{**************************************************************************************}

unit CCR.Clipboard.Consts;

interface

resourcestring
  SClipboardCoreClassMissing = 'TClipboard.TCore implementation is missing';
  SCoreClassAlreadyInitialized = 'A TClipboard.TCore implementation has already been assigned and initialized by TClipboard';
  SUnsupportedClipboardInterface = 'Unsupported clipboard interface: %s';
  SChangeNotificationsNotSupported = 'Change notifications are not supported';
  SCustomClipboardFormatsNotSupported = 'Custom clipboard formats are not supported';
  SCannotReadWhenOpenForWriting = 'A TClipboard instance cannot be read when it is open for writing';
  SCannotOpenClipboard = 'Cannot open clipboard: %s';
  SUnbalancedClose = 'Unbalanced TClipboard.Close call';
  SOrphanNewFormatSetCall = 'NewItem may only be called on an open TClipboard, and after at least one format has been assigned';
  SRTFExpected = 'Data is not valid RTF';
  SStreamCallbacksRequired = 'Stream callbacks required when clippable object does not implement IStreamPersist';
  SNoCallbacksIStreamPersistNeeded = 'Type parameter must be a IStreamPersist implementor when file extensions but no stream callbacks are provided';
  SFileNameCannotBeEmpty = 'Virtual file name cannot be blank';
  SChangeListenersNotSupported = 'Change listeners not supported';
  SDestinationDirectoryDoesNotExist = 'Destination directory does not exist (%s)';

  SUnknownClipboardFormat = 'Unsupported clipboard format';
  SNoRegisteredClipper = 'No registered clipper exists for %s or an ancestor class';

  SCannotMapBitmap = 'Map method of TBitmap failed';

  SClipboardDragDropNotSupported = 'TClipboard-based drag and drop not supported';
  SNoDataAssignedToDrag = 'No data has been assigned to drag';
  SCannotAssignPasteboardWhenOpen = 'Cannot assign Pasteboard property when TClipboard instance is open';

  SBeginClipboardDragMustBeCalledOnMouseDown = 'BeginClipboardDrag must be called in the context of a OnMouseDown event handler';

  SDropTargetMustHaveParentForm = 'Control must be on a form to be a drop target';
  SDropTargetMustHaveParent = 'Graphic control must be parented to be a drop target';

  SpasteboardWithNameFailed = 'UIPasteboard.pasteboardWithName(''%s'', %s) returned nil';

implementation

end.
