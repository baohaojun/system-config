//
// FilterPPT.cs
//
// Copyright (C) 2004 Novell, Inc.
//

//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//


using System;
using System.Collections;
using System.IO;
using System.Text;
using Gsf;

using Beagle.Daemon;
using Beagle.Util;

internal class RecordType
{

    public enum TypeCode {
	Unknown = 0,
	Document = 1000,
	DocumentAtom = 1001,
	EndDocument = 1002,
	Slide = 1006,
	SlideAtom = 1007,
	Notes = 1008,
	NotesAtom = 1009,
	Environment = 1010,
	SlidePersistAtom = 1011,
	SSlideLayoutAtom = 1015,
	MainMaster = 1016,
	SSSlideInfoAtom = 1017,
	SlideViewInfo = 1018,
	GuideAtom = 1019,
	ViewInfo = 1020,
	ViewInfoAtom = 1021,
	SlideViewInfoAtom = 1022,
	VBAInfo = 1023,
	VBAInfoAtom = 1024,
	SSDocInfoAtom = 1025,
	Summary = 1026,
	DocRoutingSlip = 1030,
	OutlineViewInfo = 1031,
	SorterViewInfo = 1032,
	ExObjList = 1033,
	ExObjListAtom = 1034,
	PPDrawingGroup = 1035, //FIXME: Office Art File Format Docu
	PPDrawing = 1036, //FIXME: Office Art File Format Docu
	NamedShows = 1040, // don't know if container
	NamedShow = 1041,
	NamedShowSlides = 1042, // don't know if container
	List = 2000,
	FontCollection = 2005,
	BookmarkCollection = 2019,
	SoundCollAtom = 2021,
	Sound = 2022,
	SoundData = 2023,
	BookmarkSeedAtom = 2025,
	ColorSchemeAtom = 2032,
	ExObjRefAtom = 3009,
	OEShapeAtom = 3009,
	OEPlaceholderAtom = 3011,
	GPointAtom = 3024,
	GRatioAtom = 3031,
	OutlineTextRefAtom = 3998,
	TextHeaderAtom = 3999,
	TextCharsAtom = 4000,
	StyleTextPropAtom = 4001,
	BaseTextPropAtom = 4002,
	TxMasterStyleAtom = 4003,
	TxCFStyleAtom = 4004,
	TxPFStyleAtom = 4005,
	TextRulerAtom = 4006,
	TextBookmarkAtom = 4007,
	TextBytesAtom = 4008,
	TxSIStyleAtom = 4009,
	TextSpecInfoAtom = 4010,
	DefaultRulerAtom = 4011,
	FontEntityAtom = 4023,
	FontEmbeddedData = 4024,
	CString = 4026,
	MetaFile = 4033,
	ExOleObjAtom = 4035,
	SrKinsoku = 4040,
	HandOut = 4041,
	ExEmbed = 4044,
	ExEmbedAtom = 4045,
	ExLink = 4046,
	BookmarkEntityAtom = 4048,
	ExLinkAtom = 4049,
	SrKinsokuAtom = 4050,
	ExHyperlinkAtom = 4051,
	ExHyperlink = 4055,
	SlideNumberMCAtom = 4056,
	HeadersFooters = 4057,
	HeadersFootersAtom = 4058,
	TxInteractiveInfoAtom = 4063,
	CharFormatAtom = 4066,
	ParaFormatAtom = 4067,
	RecolorInfoAtom = 4071,
	ExQuickTimeMovie = 4074,
	ExQuickTimeMovieData = 4075,
	ExControl = 4078,
	SlideListWithText = 4080,
	InteractiveInfo = 4082,
	InteractiveInfoAtom = 4083,
	UserEditAtom = 4085,
	CurrentUserAtom = 4086,
	DateTimeMCAtom = 4087,
	GenericDateMCAtom = 4088,
	FooterMCAtom = 4090,
	ExControlAtom = 4091,
	ExMediaAtom = 4100,
	ExVideo = 4101,
	ExAviMovie = 4102,
	ExMCIMovie = 4103,
	ExMIDIAudio = 4109,
	ExCDAudio = 4110,
	ExWAVAudioEmbedded = 4111,
	ExWAVAudioLink = 4112,
	ExOleObjStg = 4113,
	ExCDAudioAtom = 4114,
	ExWAVAudioEmbeddedAtom = 4115,
	AnimationInfoAtom = 4116,
	RTFDateTimeMCAtom = 4117,
	ProgTags = 5000, // don't know if container
	ProgStringTag = 5001,
	ProgBinaryTag = 5002,
	BinaryTagData = 5003,
	PrintOptions = 6000,
	PersistPtrFullBlock = 6001, // don't know if container
	PersistPtrIncrementalBlock = 6002, // don't know if container
	GScalingAtom = 10001,
	GRColorAtom = 10002,
	EscherDggContainer		= 0xf000, /* Drawing Group Container */
	EscherDgg			= 0xf006,
	EscherCLSID			= 0xf016,
	EscherOPT			= 0xf00b,
	EscherBStoreContainer		= 0xf001,
	EscherBSE			= 0xf007,
	EscherBlip_START		= 0xf018, /* Blip types are between */
	EscherBlip_END			= 0xf117, /* these two values */
	EscherDgContainer		= 0xf002, /* Drawing Container */
	EscherDg			= 0xf008,
	EscherRegroupItems		= 0xf118,
	EscherColorScheme		= 0xf120, /* bug in docs */
	EscherSpgrContainer		= 0xf003,
	EscherSpContainer		= 0xf004,
	EscherSpgr			= 0xf009,
	EscherSp			= 0xf00a,
	EscherTextbox			= 0xf00c,
	EscherClientTextbox		= 0xf00d,
	EscherAnchor			= 0xf00e,
	EscherChildAnchor		= 0xf00f,
	EscherClientAnchor		= 0xf010,
	EscherClientData		= 0xf011,
	EscherSolverContainer		= 0xf005,
	EscherConnectorRule		= 0xf012, /* bug in docs */
	EscherAlignRule			= 0xf013,
	EscherArcRule			= 0xf014,
	EscherClientRule		= 0xf015,
	EscherCalloutRule		= 0xf017,
	EscherSelection			= 0xf119,
	EscherColorMRU			= 0xf11a,
	EscherDeletedPspl		= 0xf11d, /* bug in docs */
	EscherSplitMenuColors		= 0xf11e,
	EscherOleObject			= 0xf11f,
	EscherUserDefined		= 0xf122,
    };

    public TypeCode  typecode;
    public string name;
    public bool    is_container;
    public bool    do_read;
    public int min_record_size;
    public int max_record_size;
    RecordType (TypeCode  typecode, string name, bool    is_container, bool    do_read, int min_record_size, int max_record_size)
    {
	this.typecode = typecode;
	this.name = name;
	this.is_container = is_container;
	this.do_read = do_read;
	this.min_record_size = min_record_size;
	this.max_record_size = max_record_size;
    }

    static RecordType[] types =
    {
	new RecordType (	TypeCode.Unknown,			"Unknown",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.Document,			"Document",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.DocumentAtom,			"DocumentAtom",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EndDocument,			"EndDocument",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.Slide,				"Slide",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.SlideAtom,			"SlideAtom",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.Notes,				"Notes",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.NotesAtom,			"NotesAtom",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.Environment,			"Environment",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.SlidePersistAtom,		"SlidePersistAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.SSlideLayoutAtom,		"SSlideLayoutAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.MainMaster,			"MainMaster",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.SSSlideInfoAtom,		"SSSlideInfoAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.SlideViewInfo,			"SlideViewInfo",		true,	true,	-1,	-1	),
	new RecordType (	TypeCode.GuideAtom,			"GuideAtom",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.ViewInfo,			"ViewInfo",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.ViewInfoAtom,			"ViewInfoAtom",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.SlideViewInfoAtom,		"SlideViewInfoAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.VBAInfo,			"VBAInfo",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.VBAInfoAtom,			"VBAInfoAtom",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.SSDocInfoAtom,			"SSDocInfoAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.Summary,			"Summary",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.DocRoutingSlip,		"DocRoutingSlip",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.OutlineViewInfo,		"OutlineViewInfo",		true,	true,	-1,	-1	),
	new RecordType (	TypeCode.SorterViewInfo,		"SorterViewInfo",		true,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExObjList,			"ExObjList",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExObjListAtom,			"ExObjListAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.PPDrawingGroup,		"PPDrawingGroup",		true,	true,	-1,	-1	), //FIXME: Office Art File Format Docu
	new RecordType (	TypeCode.PPDrawing,			"PPDrawing",			true,	true,	-1,	-1	), //FIXME: Office Art File Format Docu
	new RecordType (	TypeCode.NamedShows,			"NamedShows",			false,	true,	-1,	-1	), // don't know if container
	new RecordType (	TypeCode.NamedShow,			"NamedShow",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.NamedShowSlides,		"NamedShowSlides",		false,	true,	-1,	-1	), // don't know if container
	new RecordType (	TypeCode.List,				"List",				true,	true,	-1,	-1	),
	new RecordType (	TypeCode.FontCollection,		"FontCollection",		true,	true,	-1,	-1	),
	new RecordType (	TypeCode.BookmarkCollection,		"BookmarkCollection",		true,	true,	-1,	-1	),
	new RecordType (	TypeCode.SoundCollAtom,			"SoundCollAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.Sound,				"Sound",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.SoundData,			"SoundData",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.BookmarkSeedAtom,		"BookmarkSeedAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.ColorSchemeAtom,		"ColorSchemeAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExObjRefAtom,			"ExObjRefAtom",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.OEShapeAtom,			"OEShapeAtom",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.OEPlaceholderAtom,		"OEPlaceholderAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.GPointAtom,			"GPointAtom",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.GRatioAtom,			"GRatioAtom",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.OutlineTextRefAtom,		"OutlineTextRefAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.TextHeaderAtom,		"TextHeaderAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.TextCharsAtom,			"TextCharsAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.StyleTextPropAtom,		"StyleTextPropAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.BaseTextPropAtom,		"BaseTextPropAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.TxMasterStyleAtom,		"TxMasterStyleAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.TxCFStyleAtom,			"TxCFStyleAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.TxPFStyleAtom,			"TxPFStyleAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.TextRulerAtom,			"TextRulerAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.TextBookmarkAtom,		"TextBookmarkAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.TextBytesAtom,			"TextBytesAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.TxSIStyleAtom,			"TxSIStyleAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.TextSpecInfoAtom,		"TextSpecInfoAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.DefaultRulerAtom,		"DefaultRulerAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.FontEntityAtom,		"FontEntityAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.FontEmbeddedData,		"FontEmbeddedData",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.CString,			"CString",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.MetaFile,			"MetaFile",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExOleObjAtom,			"ExOleObjAtom",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.SrKinsoku,			"SrKinsoku",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.HandOut,			"HandOut",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExEmbed,			"ExEmbed",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExEmbedAtom,			"ExEmbedAtom",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExLink,			"ExLink",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.BookmarkEntityAtom,		"BookmarkEntityAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExLinkAtom,			"ExLinkAtom",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.SrKinsokuAtom,			"SrKinsokuAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExHyperlinkAtom,		"ExHyperlinkAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExHyperlink,			"ExHyperlink",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.SlideNumberMCAtom,		"SlideNumberMCAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.HeadersFooters,		"HeadersFooters",		true,	true,	-1,	-1	),
	new RecordType (	TypeCode.HeadersFootersAtom,		"HeadersFootersAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.TxInteractiveInfoAtom,		"TxInteractiveInfoAtom",	false,	true,	-1,	-1	),
	new RecordType (	TypeCode.CharFormatAtom,		"CharFormatAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.ParaFormatAtom,		"ParaFormatAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.RecolorInfoAtom,		"RecolorInfoAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExQuickTimeMovie,		"ExQuickTimeMovie",		true,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExQuickTimeMovieData,		"ExQuickTimeMovieData",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExControl,			"ExControl",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.SlideListWithText,		"SlideListWithText",		true,	true,	-1,	-1	),
	new RecordType (	TypeCode.InteractiveInfo,		"InteractiveInfo",		true,	true,	-1,	-1	),
	new RecordType (	TypeCode.InteractiveInfoAtom,		"InteractiveInfoAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.UserEditAtom,			"UserEditAtom",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.CurrentUserAtom,		"CurrentUserAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.DateTimeMCAtom,		"DateTimeMCAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.GenericDateMCAtom,		"GenericDateMCAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.FooterMCAtom,			"FooterMCAtom",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExControlAtom,			"ExControlAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExMediaAtom,			"ExMediaAtom",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExVideo,			"ExVideo",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExAviMovie,			"ExAviMovie",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExMCIMovie,			"ExMCIMovie",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExMIDIAudio,			"ExMIDIAudio",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExCDAudio,			"ExCDAudio",			true,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExWAVAudioEmbedded,		"ExWAVAudioEmbedded",		true,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExWAVAudioLink,		"ExWAVAudioLink",		true,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExOleObjStg,			"ExOleObjStg",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExCDAudioAtom,			"ExCDAudioAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.ExWAVAudioEmbeddedAtom,	"ExWAVAudioEmbeddedAtom",	false,	true,	-1,	-1	),
	new RecordType (	TypeCode.AnimationInfoAtom,		"AnimationInfoAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.RTFDateTimeMCAtom,		"RTFDateTimeMCAtom",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.ProgTags,			"ProgTags",			false,	true,	-1,	-1	), // don't know if container
	new RecordType (	TypeCode.ProgStringTag,			"ProgStringTag",		true,	true,	-1,	-1	),
	new RecordType (	TypeCode.ProgBinaryTag,			"ProgBinaryTag",		true,	true,	-1,	-1	),
	new RecordType (	TypeCode.BinaryTagData,			"BinaryTagData",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.PrintOptions,			"PrintOptions",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.PersistPtrFullBlock,		"PersistPtrFullBlock",		false,	true,	-1,	-1	), // don't know if container
	new RecordType (	TypeCode.PersistPtrIncrementalBlock,	"PersistPtrIncrementalBlock",	false,	true,	-1,	-1	),
	new RecordType (	TypeCode.GScalingAtom,			"GScalingAtom",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.GRColorAtom,			"GRColorAtom",			false,	true,	-1,	-1	),

	new RecordType (	TypeCode.EscherDggContainer,		"EscherDggContainer",		true,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherDgg,			"EscherDgg",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherCLSID,			"EscherCLSID",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherOPT,			"EscherOPT",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherBStoreContainer,		"EscherBStoreContainer",	true,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherBSE,			"EscherBSE",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherBlip_START,		"EscherBlip_START",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherBlip_END,		"EscherBlip_END",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherDgContainer,		"EscherDgContainer",		true,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherDg,			"EscherDg",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherRegroupItems,		"EscherRegroupItems",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherColorScheme,		"EscherColorScheme",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherSpgrContainer,		"EscherSpgrContainer",		true,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherSpContainer,		"EscherSpContainer",		true,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherSpgr,			"EscherSpgr",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherSp,			"EscherSp",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherTextbox,			"EscherTextbox",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherClientTextbox,		"EscherClientTextbox",		true,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherAnchor,			"EscherAnchor",			false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherChildAnchor,		"EscherChildAnchor",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherClientAnchor,		"EscherClientAnchor",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherClientData,		"EscherClientData",		true,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherSolverContainer,		"EscherSolverContainer",	true,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherConnectorRule,		"EscherConnectorRule",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherAlignRule,		"EscherAlignRule",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherArcRule,			"EscherArcRule",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherClientRule,		"EscherClientRule",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherCalloutRule,		"EscherCalloutRule",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherSelection,		"EscherSelection",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherColorMRU,		"EscherColorMRU",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherDeletedPspl,		"EscherDeletedPspl",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherSplitMenuColors,		"EscherSplitMenuColors",	false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherOleObject,		"EscherOleObject",		false,	true,	-1,	-1	),
	new RecordType (	TypeCode.EscherUserDefined,		"EscherUserDefined",		false,	true,	-1,	-1	)
    };

    public static RecordType Find (TypeCode typecode)
    {
	for (int i = 0; i < types.Length; i++) {
	    if (types[i].typecode == typecode)
		return types[i];
	}
	return types[0];
    }
}

namespace Beagle.Filters {
    
	public class FilterPPT : FilterOle {

		private enum TextType {
			Invalid = -1,
			Title,
			Body,
			Notes,
			NotUsed,
			Other,
			CenterBody,
			CenterTitle,
			HalfBody,
			QuarterBody
		};

		TextType textType;
		public FilterPPT () 
		{
			textType = TextType.Invalid;
			file = null;
			FileName = null;
			SnippetMode = true;
			SetFileType ("document");
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/vnd.ms-powerpoint"));
		}

		private int ParseElement (Input stream)
		{
			int data_remaining = (int) stream.Remaining;
			//Console.WriteLine ("stream.Remaining = {0}", data_remaining);

			// Weird!! Well, Its a M$ format ;-)
			// Fixes: 323312
			byte [] data = stream.Read (data_remaining > 7 ? 8 : data_remaining);
			if (data == null || data_remaining < 8)
				return 0;

			RecordType.TypeCode opcode = (RecordType.TypeCode) GetInt16(data, 2);
			int length = (int)GetInt32(data, 4);
			// Protect against garbage length
			length = (length > 0 ? length : 0);
			RecordType type = RecordType.Find (opcode);

			// Process the container tree
			if (type.is_container) {
				int length_remaining = length;

				if (opcode == RecordType.TypeCode.MainMaster) {
					// Ignore MainMaster container as it contains
					// just a master-slide view and no user data.
					stream.Seek (length_remaining, SeekOrigin.Current);
				} else {
					while (length_remaining > 0) {
						int elem_length = ParseElement(stream);
						if (elem_length == 0)
							return 0;
						length_remaining -= elem_length;
						//Console.WriteLine ("ParseElement: length = {0}, rem = {1}", 
						//		   elem_length, length_remaining);
					}
				}
			} else {
				if (length != 0) {
					System.Text.Encoding encoding = null;

					if (opcode == RecordType.TypeCode.TextBytesAtom) {
						//encoding = System.Text.Encoding.GetEncoding (28591);
						encoding = System.Text.Encoding.UTF8;
					} else if (opcode == RecordType.TypeCode.TextCharsAtom) {
						encoding = System.Text.Encoding.Unicode;
					}
					
					if (encoding != null && textType != TextType.NotUsed) {
						StringBuilder strData = new StringBuilder () ;
						data = stream.Read(length);
						if (data == null)
							return 0;
						// Replace all ^M with "whitespace",
						// because of which the contents were not properly 
						// been appended to the text pool.
						strData.Append (encoding.GetString (data).Replace ('\r', ' '));

						// Replace all ^K with "whitespace",
						// because of which the contents were not properly 
						// been appended to the text pool.
						strData.Replace ((char)0x0B, (char)0x20);
						
						if (textType == TextType.Title ||
						    textType == TextType.CenterBody ||
						    textType == TextType.CenterTitle)
							HotUp ();
						AppendText (strData.ToString());
						if (IsHot)
							HotDown ();
						AppendStructuralBreak ();
						//Console.WriteLine ("Text : {0}", strData);
					}  else if (opcode == RecordType.TypeCode.TextHeaderAtom) {
						data = stream.Read (4);
						textType = (TextType) GetInt32 (data, 0);
					} else {
						stream.Seek(length, SeekOrigin.Current);
					}
				}
			}

			// length = RecordHeader.recLen
			// 8 = sizeof (RecordHeader)
			// Every Atom/container is preceded by a RecordHeader
			return length + 8;
		}
		

		override protected void ExtractMetaData (Input sumStream, Input docSumStream)
		{
			int slide_count = 0;
			DocProp prop = null;

			if (docSumMeta != null) {
				prop = docSumMeta.Lookup ("gsf:slide-count");
				if (prop != null)
					slide_count = (int) prop.Val;
				if (slide_count > 0)
					AddProperty (Beagle.Property.NewUnsearched ("fixme:slide-count", slide_count));
			}
		}

		override protected void DoPull ()
		{
			if (file == null) {
				Finished ();
				return;
			}

			Input stream = null;
			try {
				stream = file.ChildByName ("PowerPoint Document");

				if (stream != null) {

					// The parsing was getting terminated when "EndDocument"
					// container was parsed.  We need to continue our 
					// parsing till the end of the file, since, some of the
					// slides do persist after the actual "Document" 
					// container.
					// PPTs exported from OO.o actually writes almost all the slides
					// after "Document" container.
					// And certain PPTs do have some slides in after
					// "Document" container.
					//Console.WriteLine ("Length of stream = {0}", stream.Size);
					int ret = -1;
					while (!stream.Eof && ret != 0) {
						ret = ParseElement (stream);
						//Console.WriteLine ("Position of the ptr in the stream: {0}", stream.Position);
					}
					
					stream.Dispose ();
				} else {
					Logger.Log.Error ("Ole stream not found in {0}.  Content extraction skipped.", FileName);
				}

				Finished ();
			} catch (Exception e) {
				Logger.Log.Error ("Exception {0} occurred during DoPull.", e.Message);
				Error ();
			}
		}

		override protected void OpenStorage (FileInfo info)
		{
			FileName = info.FullName;

			// PPT 95/97-2000 format contains a "PP97_DUALSTORAGE", which is required
			// to index PPT 97-2000 files.
			// We don't support PPT 95 files, however, we happily accept patches ;-)
			
			Input temp_input = null;
			try {
				temp_input = file.ChildByName ("PP97_DUALSTORAGE");
				
				if (temp_input != null) {
					// "PP97_DUALSTORAGE" is a storage containing some streams
					if (temp_input.Handle != IntPtr.Zero) {
						file.Dispose ();
						file = (Gsf.Infile) GLib.Object.GetObject (temp_input.Handle);
					}
				} else {
					using (temp_input = file.ChildByName ("Header")) {
						using (Input temp_input2 = file.ChildByName ("PowerPoint Document")) {
							if (temp_input != null || temp_input2 == null) {
								Log.Error ("{0} is an unsupported PPT 95/4.0 file.  Skipping", FileName);
								Error ();
								return;
							}
						}
					}
				}	
				
				using (temp_input = file.ChildByName ("EncryptedSummary")) {
					if (temp_input != null) {
						Log.Warn ("{0} is a password-protected PowerPoint file.  Skipping.", FileName);
						Error ();
					}
				}

			} catch (Exception) {
				
				Logger.Log.Error ("Unable to open OleFile stream of "+info.FullName);
				Error ();
			}
		}
	}
}
