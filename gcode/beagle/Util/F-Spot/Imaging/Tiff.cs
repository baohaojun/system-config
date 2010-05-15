//#define DEBUG_LOADER
using FSpot;
using SemWeb;
using System;
using System.IO;
using System.Collections.Generic;

#if ENABLE_NUNIT
using NUnit.Framework;
#endif

namespace FSpot.Tiff {

	// This is primarily to preserve the names from the specification
	// because they differ from the tiff standard names
	public enum NiffId : ushort {
		SubfileType                     = 0x00fe,
		PelPathLength                   = 0x0100,
		LineProgressionLength           = 257,
		BitsPerSample                   = 0x0101,
		PhotometricInterpretation       = 0x0106,
		DataOffset                      = 0x0111,
		SamplesPerPixel 		= 0x0115,
		DataByteCounts                  = 0x0117,
		PelPathResolution               = 0x011a,
		LineProgressionResolution       = 0x011b,
		ResolutionUnit  		= 0x0128,
		ColumnsPerPelPath               = 322,
		RowsPerLineProgression          = 323,
	        Rotation                        = 33465,
	        NavyCompression                 = 33466,
		TileIndex                       = 33467
	}

	public enum TagGPS : ushort {
		GPSVersionID                    = 0x0000,
		GPSLatitudeRef                  = 0x0001,
		GPSLatitude                     = 0x0002,
			GPSLongitudeRef                    = 0x0003,
			GPSLongitue                        = 0x0004,
			GPSAltitudeRef                     = 0x0005,
			GPSAltitude                        = 0x0006,
			GPSTimeStamp                       = 0x0007,
			GPSSatellites                      = 0x0008,
			GPSStatus                          = 0x0009,
			GPSMeasureMode                     = 0x000a,
			GPSDOP                             = 0x000b,
			GPSSpeedRef                        = 0x000c,
			GPSSpeed                           = 0x000d,
			GPSTrackRef                        = 0x000e,
			GPSTrack                           = 0x000f,
			GPSImgDirectionRef                 = 0x0010,
			GPSImgDirection                    = 0x0011,
			GPSMapDatum                        = 0x0012,
			GPSDestLatitudeRef                 = 0x0013,
			GPSDestLatitude                    = 0x0014,
			GPSDestLongitudeRef                = 0x0015,
			GPSDestLongitude                   = 0x0016,
			GPSDestBearingRef                  = 0x0017,
			GPSDestBearing                     = 0x0018,
			GPSDestDistanceRef                 = 0x0019,
			GPSDestDistance                    = 0x001a,
			GPSProcessingMethod                = 0x001b,
			GPSAreaInformation                 = 0x001c,
			GPSDateStamp                       = 0x001d,
			GPSDifferential                    = 0x001e
	}

	public enum TagId : ushort {
		InteroperabilityIndex		= 0x0001,
		InteroperabilityVersion	        = 0x0002,
		
		NewSubfileType                  = 0x00fe,
		SubfileType                     = 0x00ff,
		
		ImageWidth 			= 0x0100,
		ImageLength 			= 0x0101,
		BitsPerSample 	         	= 0x0102,
		Compression 			= 0x0103,
		PhotometricInterpretation 	= 0x0106,
		FillOrder 			= 0x010a,
		DocumentName 			= 0x010d,
		ImageDescription 		= 0x010e,
		Make 				= 0x010f,
		Model 				= 0x0110,
		StripOffsets 			= 0x0111,
		Orientation 			= 0x0112,
		SamplesPerPixel 		= 0x0115,
		RowsPerStrip    		= 0x0116,
		StripByteCounts 		= 0x0117,
		XResolution 			= 0x011a,
		YResolution 			= 0x011b,
		PlanarConfiguration 		= 0x011c,

		T4Options                       = 0x0124,
		T6Options                       = 0x0125,

		ResolutionUnit  		= 0x0128,
		TransferFunction 		= 0x012d,
		Software 			= 0x0131,
		DateTime			= 0x0132,
		Artist				= 0x013b,
		WhitePoint			= 0x013e,
		PrimaryChromaticities		= 0x013f,
			
		HalftoneHints                   = 0x0141,
		// Tiled images
		TileWidth                       = 0x0142,
		TileLength                      = 0x0143,
		TileOffsets                     = 0x0144,
	        TileByteCounts                  = 0x0145,

		SubIFDs                         = 0x014a, // TIFF-EP

		// CMYK images
		InkSet                          = 0x014c,
		NumberOfInks                    = 0x014e,
	        InkNames                        = 0x014d,
		DotRange                        = 0x0150,
		TargetPrinter                   = 0x0151,
		ExtraSamples                    = 0x0152,
		SampleFormat                    = 0x0153,
		SMinSampleValue                 = 0x0154,
		SMaxSampleValue                 = 0x0155,
		
		TransferRange			= 0x0156,
		
		ClipPath                        = 0x0157, // TIFF PageMaker Technote #2.
		
		JPEGTables                      = 0x015b, // TIFF-EP
		
		JPEGProc			= 0x0200,
		JPEGInterchangeFormat	        = 0x0201,
		JPEGInterchangeFormatLength	= 0x0202,
	        JPEGRestartInterval             = 0x0203,
	        JPEGLosslessPredictors          = 0x0205,
		JPEGPointTransforms             = 0x0206,
		JPEGQTables                     = 0x0207,
		JPEGDCTables                    = 0x0208,
		JPEGACTables                    = 0x0209,

		YCbCrCoefficients		= 0x0211,
		YCbCrSubSampling		= 0x0212,
		YCbCrPositioning		= 0x0213,

		ReferenceBlackWhite		= 0x0214,
		RelatedImageFileFormat   	= 0x1000,
		RelatedImageWidth		= 0x1001,
		RelatedImageLength		= 0x1002,
		CFARepeatPatternDim		= 0x828d,
		CFAPattern			= 0x828e,
		BatteryLevel			= 0x828f,
		Copyright			= 0x8298,
		ExposureTime			= 0x829a,
		FNumber 			= 0x829d,

		// These are from the NIFF spec and only really valid when the header begins with IIN1
		// see the NiffTag enum for the specifcation specific names
			Rotation                        = 0x82b9,
			NavyCompression                 = 0x82ba,
			TileIndex                       = 0x82bb,
		// end NIFF specific
			
		IPTCNAA	        		= 0x83bb,

		PhotoshopPrivate                = 0x8649,

		ExifIfdPointer      		= 0x8769,
		InterColorProfile		= 0x8773,
		ExposureProgram 		= 0x8822,
		SpectralSensitivity		= 0x8824,
		GPSInfoIfdPointer		= 0x8825,
		ISOSpeedRatings	        	= 0x8827,
		OECF				= 0x8828,
		ExifVersion			= 0x9000,
		DateTimeOriginal		= 0x9003,
		DateTimeDigitized		= 0x9004,
		ComponentsConfiguration	        = 0x9101,
		CompressedBitsPerPixel	        = 0x9102,
		ShutterSpeedValue		= 0x9201,
		ApertureValue			= 0x9202,
		BrightnessValue  		= 0x9203,
		ExposureBiasValue		= 0x9204,
		MaxApertureValue		= 0x9205,
		SubjectDistance 		= 0x9206,
		MeteringMode			= 0x9207,
		LightSource			= 0x9208,
		Flash				= 0x9209,
		FocalLength			= 0x920a,
			
		FlashEnergy_TIFFEP              = 0x920b,// TIFF-EP 
		SpacialFrequencyResponse        = 0x920c,// TIFF-EP 
		Noise                           = 0x920d,// TIFF-EP 
		FocalPlaneXResolution_TIFFEP    = 0x920e,// TIFF-EP 
		FocalPlaneYResolution_TIFFEP    = 0x920f,// TIFF-EP 
		FocalPlaneResolutionUnit_TIFFEP = 0x9210,// TIFF-EP 
		ImageName                       = 0x9211,// TIFF-EP 
		SecurityClassification          = 0x9212,// TIFF-EP 
		
		ImageHistory                    = 0x9213, // TIFF-EP null separated list

	        SubjectArea			= 0x9214,

		ExposureIndex_TIFFEP            = 0x9215, // TIFF-EP
		TIFFEPStandardID                = 0x9216, // TIFF-EP
		SensingMethod_TIFFEP            = 0x9217, // TIFF-EP
			
		MakerNote			= 0x927c,
		UserComment			= 0x9286,
		SubSecTime			= 0x9290,
		SubSecTimeOriginal		= 0x9291,
		SubSecTimeDigitized		= 0x9292,
		FlashPixVersion 		= 0xa000,
		ColorSpace			= 0xa001,
		PixelXDimension 		= 0xa002,
		PixelYDimension 		= 0xa003,
		RelatedSoundFile		= 0xa004,
		InteroperabilityIfdPointer	= 0xa005,
		FlashEnergy			= 0xa20b,
		SpatialFrequencyResponse	= 0xa20c,
		FocalPlaneXResolution	        = 0xa20e,
		FocalPlaneYResolution	        = 0xa20f,
		FocalPlaneResolutionUnit	= 0xa210,
		SubjectLocation 		= 0xa214,
		ExposureIndex			= 0xa215,
		SensingMethod			= 0xa217,
		FileSource			= 0xa300,
		SceneType			= 0xa301,
		ExifCFAPattern		        = 0xa302,
		CustomRendered  		= 0xa401,
		ExposureMode			= 0xa402,
		WhiteBalance			= 0xa403,
		DigitalZoomRatio		= 0xa404,
		FocalLengthIn35mmFilm	        = 0xa405,
		SceneCaptureType		= 0xa406,
		GainControl			= 0xa407,
		Contrast			= 0xa408,
		Saturation			= 0xa409,
		Sharpness			= 0xa40a,
		DeviceSettingDescription	= 0xa40b,
		SubjectDistanceRange		= 0xa40c,
		ImageUniqueId   		= 0xa420,

		// The Following IDs are not described the EXIF spec
		Gamma                           = 0xa500,

		// The XMP spec declares that XMP data should live 0x2bc when
		// embedded in tiff images.
		XMP                             = 0x02bc,
		
		// from the dng spec
		DNGVersion                      = 0xc612, // Ifd0
		DNGBackwardVersion              = 0xc613, // Ifd0
		UniqueCameraModel               = 0xc614, // Ifd0
		LocalizedCameraModel            = 0xc615, // Ifd0
		CFAPlaneColor                   = 0xc616, // RawIfd
		CFALayout                       = 0xc617, // RawIfd
		LinearizationTable              = 0xc618, // RawIfd
		BlackLevelRepeatDim             = 0xc619, // RawIfd
		BlackLevel                      = 0xc61a, // RawIfd
		BlackLevelDeltaH                = 0xc61b, // RawIfd
		BlackLevelDeltaV                = 0xc61c, // RawIfd
		WhiteLevel                      = 0xc61d, // RawIfd
		DefaultScale                    = 0xc61e, // RawIfd		
		DefaultCropOrigin               = 0xc61f, // RawIfd
		DefaultCropSize                 = 0xc620, // RawIfd
		ColorMatrix1                    = 0xc621, // Ifd0
		ColorMatrix2                    = 0xc622, // Ifd0
		CameraCalibration1              = 0xc623, // Ifd0
		CameraCalibration2              = 0xc624, // Ifd0
		ReductionMatrix1                = 0xc625, // Ifd0
		ReductionMatrix2                = 0xc626, // Ifd0
		AnalogBalance                   = 0xc627, // Ifd0
		AsShotNetural                   = 0xc628, // Ifd0
		AsShotWhiteXY                   = 0xc629, // Ifd0
		BaselineExposure                = 0xc62a, // Ifd0
		BaselineNoise                   = 0xc62b, // Ifd0
		BaselineSharpness               = 0xc62c, // Ifd0
		BayerGreeSpit                   = 0xc62d, // Ifd0
		LinearResponseLimit             = 0xc62e, // Ifd0
		CameraSerialNumber              = 0xc62f, // Ifd0
		LensInfo                        = 0xc630, // Ifd0
		ChromaBlurRadius                = 0xc631, // RawIfd
		AntiAliasStrength               = 0xc632, // RawIfd
		DNGPrivateData                  = 0xc634, // Ifd0
		
		MakerNoteSafety                 = 0xc635, // Ifd0

		// The Spec says BestQualityScale is 0xc635 but it appears to be wrong
		//BestQualityScale                = 0xc635, // RawIfd 
		BestQualityScale                = 0xc63c, // RawIfd  this looks like the correct value

		CalibrationIlluminant1          = 0xc65a, // Ifd0
		CalibrationIlluminant2          = 0xc65b, // Ifd0
		
		// Print Image Matching data
		PimIfdPointer                   = 0xc4a5
	}

	public struct SRational {
		public int Numerator;
		public int Denominator;
		
		public SRational (byte [] raw_data, int offset, bool little)
		{
			Numerator = BitConverter.ToInt32 (raw_data, offset, little);
			Denominator = BitConverter.ToInt32 (raw_data, offset, little);
		}

		public SRational (int numerator, int denominator)
		{
			Numerator = numerator;
			Denominator = denominator;
		}

		public static SRational BitwiseCopy (Rational rational)
		{
			SRational result;

			result.Numerator = unchecked ((int) rational.Numerator);
			result.Denominator = unchecked ((int) rational.Denominator);
			return result;
		}

		public override string ToString ()
		{
			if (Numerator == 0 || Denominator == 0)
				return String.Format ("{0}/{1}", Numerator, Denominator);
			else if (Numerator % Denominator == 0)
				return String.Format ("{0}", Numerator / Denominator);
			else if (Denominator % Numerator == 0)
				return String.Format ("1/{0}", Denominator / Numerator);

			return String.Format ("{0}/{1}", Numerator, Denominator);
		}
		
		public double Value {
			get {
				return Numerator / (double)Denominator;
			}
		}
	}

	public struct Rational {
		public uint Numerator;
		public uint Denominator;

		public Rational (uint numerator, uint denominator)
		{
			Numerator = numerator;
			Denominator = denominator;
		}
		
		public Rational (string value)
		{
			string [] vals = value.Split ('/');
			if (vals.Length == 2) {
				this.Numerator = UInt32.Parse (vals [0]);
				this.Denominator = UInt32.Parse (vals [1]);
				return;
			} if (vals.Length == 1) {
				double tmp = Double.Parse (value);
				this.Numerator = (uint) (tmp * 100000);
				this.Denominator = 100000;
			} else
				throw new ParseException ("unable to parse rational value");
		}

		public override string ToString ()
		{
			if (Numerator == 0 || Denominator == 0)
				return String.Format ("{0}/{1}", Numerator, Denominator);
			else if (Numerator % Denominator == 0)
				return String.Format ("{0}", Numerator / Denominator);
			else if (Denominator % Numerator == 0)
				return String.Format ("1/{0}", Denominator / Numerator);

			return String.Format ("{0}/{1}", Numerator, Denominator);
		}
		
		public double Value {
			get {
				return Numerator / (double)Denominator;
			}
		}
	}

	struct UserComment {
		string Charset;
		public string Value;

		public UserComment (string value)
		{
			Charset = null;
			Value = value;
		}

		public UserComment (byte [] raw_data, bool little)
		{
			if (raw_data.Length == 8 || raw_data.Length == 0) { 
				Charset = null;
				Value = String.Empty;
				return;
			} else if (raw_data.Length < 8) {
				throw new Exception ("Invalid UserComment value, no charset found");
			}

			string charset = System.Text.Encoding.ASCII.GetString (raw_data, 0, 8);
			System.Text.Encoding enc;

			switch (charset) {
			case "ASCII\0\0\0":
				enc = System.Text.Encoding.ASCII;
				break;
			case "UNICODE\0":
			case "Unicode\0":
				enc = new System.Text.UnicodeEncoding (! little, true);
				break;
			case "JIS\0\0\0\0\0":
				// FIXME this requires mono locale extras
				try {
					enc = System.Text.Encoding.GetEncoding ("euc-jp");
				} catch {
					//System.Console.WriteLine ("missing jis0208 encoding");
					enc = System.Text.Encoding.Default;
				}
				break;
			case "\0\0\0\0\0\0\0\0":
				// FIXME the spec says to use the local encoding in this case, we could probably
				// do something smarter, but whatever.
				enc = System.Text.Encoding.Default;
				break;
			default:
				enc = null;
				throw new ParseException (System.String.Format ("Invalid charset name: {0}", charset));
			}

			Charset = charset;
			// for (int i = 0; i < raw_data.Length; i++)
			//	System.Console.WriteLine ("{0} - \"{1}\"", raw_data [i].ToString ("x"), raw_data [i]);
			Value = enc.GetString (raw_data, 8, raw_data.Length - 8).Trim('\0');
		}

		public byte [] GetBytes (bool is_little)
		{
			bool ascii = true;
			string description = Value;
			System.Text.Encoding enc;
			string heading;

			for (int i = 0; i < description.Length; i++) {
				if (description [i] > 127) {
					ascii = false;
					break;
				}
			}

			if (ascii) {
				heading = "ASCII\0\0\0";
				enc = new System.Text.ASCIIEncoding ();
			} else {
				heading = "Unicode\0";
				enc = new System.Text.UnicodeEncoding (! is_little, true);
			}
			
			int len = enc.GetByteCount (description);
			byte [] data = new byte [len + heading.Length];
			System.Text.Encoding.ASCII.GetBytes (heading, 0, heading.Length, data, 0);
			enc.GetBytes (Value, 0, Value.Length, data, heading.Length);
			
			UserComment c = new UserComment (data, is_little);
			//System.Console.WriteLine ("old = \"{0}\" new = \"{1}\" heading = \"{2}\"", c.Value, description, heading);
			return data;
		}

		public override string ToString ()
		{
			return String.Format ("({0},charset={1})", Value, Charset);
		}
	}

	public struct CFAPattern {
		public ushort Rows;
		public ushort Columns;
		public byte [] Values;

		public CFAPattern (byte [] raw_data, bool little)
		{
			Columns = BitConverter.ToUInt16 (raw_data, 0, little);
			Rows = BitConverter.ToUInt16 (raw_data, 2, little);

			Values = new byte [Rows * Columns];
			// FIXME the contents here may differ from what we
			// expect, but for now we won't throw an exception
			// since the entry still may be a valid structure.
			if (raw_data.Length -4 < Values.Length)
				System.Array.Copy (raw_data, 4, Values, 0, Values.Length);
		}

		/* 
		 * Note the Exif spec defines a CFA pattern tag that includes the row and column counts as shorts
		 * inside the first four bytes of the entry.  The Tiff-EP standard define the CFARepeatPattern tag
		 * that contains the row and column counts presumably since the Exif version wouldn't allow you to
		 * alter the endian of the file without knowing the tag layout.
		 */
		
		public CFAPattern (ushort rows, ushort cols, byte [] raw_data,  bool little)
		{
			Columns = rows;
			Rows = cols;
			Values = new byte [rows * cols];
			System.Array.Copy (raw_data, 0, Values, 0, Values.Length);
		}
	}

	public struct OECFTable {
		public ushort Rows;
		public ushort Columns;
		public string [] Names;
		public SRational [] Values;
		
		public OECFTable (byte [] raw_data, bool little)
		{
			Columns = BitConverter.ToUInt16 (raw_data, 0, little);
			Rows = BitConverter.ToUInt16 (raw_data, 2, little);
			Names = new string [Columns];
			Values = new SRational [Columns * Rows];

			int pos = 2;
			int i;
			int type_size = DirectoryEntry.GetTypeSize (EntryType.SRational);

			for (i = 0; i < Names.Length; i++)
				Names [i] = ReadString (raw_data, ref pos);
			
			for (i = 0; i < Values.Length; i++)
				Values [i] = new SRational (raw_data, pos + i * type_size, little);
			
		}

		public string ReadString (byte [] data, ref int pos)
		{
			int start = pos;
			for (; pos < data.Length; pos++) {
				if (data [pos] == 0)
					break;
			}	
			return System.Text.Encoding.ASCII.GetString (data, start, pos - start);
		}
	}

	public enum ExtraSamples {
		Unspecified = 0,
		AssociatedAlpha = 1,
		UnassociatedAlpa = 2
	}

	public enum FileSource {
		DCF = 3,
	}

	public enum PhotometricInterpretation : ushort {
		WhiteIsZero = 0,
		BlackIsZero = 1,
		RGB = 2,
		PaletteColor = 3,
		TransparencyMask = 4,
		Separated = 5,  // CMYK
		YCbCr = 6,
		CIELab = 8,
		ICCLab = 9,
		ITULab = 10,
		LogL = 32844, // Log Luminance
		LogLUV = 32845,
		ColorFilterArray = 32803,  // ColorFilterArray... the good stuff
		LinearRaw = 34892  // DBG LinearRaw
	}

	public enum PlanarConfiguration {
		Chunky = 1,
		Planar = 2
	}
	
	public enum Compression {
		Packed = 1,
		Huffman = 2,
		T4 = 3,
		T6 = 4,
		LZW = 5,
		JPEG = 6,
		JPEGStream = 7,  // TIFF-EP stores full jpeg stream 
		Deflate = 8,
		JBIG = 9,
		JBIG_MRC,
		PackBits = 32773,
		NikonCompression = 34713,
		Deflate_experimental = 0x80b2
	}

	public enum JPEGProc {
		BaselineSequencial = 1,
		LosslessHuffman = 14,
	}

	public enum SubfileType {
		FullResolution = 1,
		ReducedResolution = 2,
		PageOfMultipage = 3
	}
	
	public enum ExposureProgram {
		NotDefined = 0,
		Manual = 1,
		NormalProgram = 2,  // Normal Program
		AperturePriority = 3, // Aperture priority
		ShutterPriorty = 4, // Shutter priority
		CreativeProgram = 5, // Creative program
		ActionProgram = 6, // Action program
		PortraitMode = 7, // Portrait mode
		LandscapeMode = 8 // Landscape mode
	}

	public enum ExposureMode {
		Auto = 0,
		Manual = 1,
		AutoBracket = 2
	}

	public enum CustomRendered : ushort {
		Normal = 0,
		Custom = 1
	}

	public enum SceneType {
		DirectlyPhotographed = 1
	}

	public enum MeteringMode {
		Uknown = 0,
		Average = 1,
		CenterWeightedAverage = 2,
		Spot = 3,
		MulitSpot = 4,
		Pattern = 5,
		Partial = 6,
	}

	public enum SceneCaptureType : ushort {
		Standard = 0,
		Landscape = 1,
		Portrait = 2,
		NightScene = 3
	}

	public enum GainControl : ushort {
		None = 0,
		LowGainUp = 1,
		HighGainUp = 2,
		LowGainDown = 3,
		HighGainDown = 4
	}

	public enum Contrast : ushort {
		Normal = 0,
		Soft = 1,
		Hard = 2
	}
	
	public enum Saturation : ushort {
		Normal = 0,
		Low = 1,
		High = 2
	}

	public enum WhiteBalance : ushort {
		Auto = 0,
	        Manual = 1
	}

	public enum Sharpness : ushort {
		Normal = 0,
		Soft = 1,
		Hard = 2
	}

	public enum LightSource {
		Unknown = 0,
		Daylight = 1,
		Fluorescent = 2,
		Tungsten = 3,
		Fash = 4,
		FineWeather = 9,
		CloudyWeather = 10,
		Shade = 11,
		DaylightFluorescent = 12,
		DaylightWhiteFluorescent = 13,
		CoolWhiteFluorescent = 14,
		WhiteFluorescent = 15,
		StandardLightA = 17,
		StandardLightB = 18,
		StandardLightC = 19,
		D55 = 20,
		D65 = 21,
		D75 = 22,
		D50 = 23,
		ISOStudioTungsten = 24,
		OtherSource = 255
	}

	public enum ColorSpace {
		StandardRGB = 1,  // sRGB
		AdobeRGB = 2,
		Uncalibrated = 0xffff
	}

	public enum ComponentsConfiguration {
		DoesNotExist = 0,
		Y = 1,
		Cb = 2,
		Cr = 3,
		R = 4,
		G = 6,
	}

	public enum ResolutionUnit : ushort {
	        Uncalibrated = 1,
		Inch = 2,
		Centimeter = 3
	}
	
	public enum SensingMethod : short {
		NotDefined = 1,
		OneChipColorAreaSensor = 2,
		TwoChipColorAreaSensor = 3,
		ThreeChipColorAreaSensor = 4,
		ColorSequentialAreaSensor = 5,
		TrilinearSensor = 7,
		ColorSequentialLinearSensor = 8
	}

	[System.Flags]
	public enum NewSubfileType : uint {
		ReducedResolution = 1,
		PageOfMultipage= 1 << 1,
		TransparencyMask = 1 << 2
	}

	public enum EntryType {
		Byte = 1,
		Ascii,
		Short,
		Long,
		Rational,
		SByte,
		Undefined,
		SShort,
		SLong,
		SRational,
		Float,
		Double,
		Ifd // TIFF-EP - TIFF PageMaker TechnicalNote 2
	}
	
	public class Tag {
		public ushort Id;
		public EntryType Type;
		public int Count;
		public string Name;
		public string Description;
	}

	public class CanonTag : Tag {
		// http://www.gvsoft.homedns.org/exif/makernote-canon.html
		
		public enum CanonId {
			Unknown1           = 0x0000,
			CameraSettings1    = 0x0001,
			Unknown2           = 0x0003,
			CameraSettings2    = 0x0004,
			ImageType          = 0x0006,
			FirmwareVersion    = 0x0007,
			ImageNumber        = 0x0008,
			OwnerName          = 0x0009,
			Unknown3           = 0x000a,
			CameraSerialNumber = 0x000c,
			Unknown4           = 0x000d,
			CustomFunctions    = 0x000f
		}
		
		public CanonTag (CanonId id, EntryType type, int count, string name, string description)
		{
			this.Id = (ushort)id;
			this.Type = type;
			this.Count = count;
			this.Name = name;
			this.Description = description;
		}

		public static System.Collections.Hashtable Tags;

		static CanonTag () {
			CanonTag [] tags = { 
				new CanonTag (CanonId.Unknown1, EntryType.Short, 6, null, null),
				new CanonTag (CanonId.CameraSettings1, EntryType.Short, -1, "Camera Settings 1", "First Canon MakerNote settings section"),
				new CanonTag (CanonId.Unknown2, EntryType.Short, 4, null, null),				
				new CanonTag (CanonId.CameraSettings2, EntryType.Short, -1, "Camera Settings 2", "Second Canon MakerNote settings section"),
				new CanonTag (CanonId.ImageType, EntryType.Ascii, 32, "Image Type", null), // FIXME description
				new CanonTag (CanonId.FirmwareVersion, EntryType.Ascii, 24, "Firmware Version", "Version of the firmware installed on the camera"),
				new CanonTag (CanonId.ImageNumber, EntryType.Long, 1, "Image Number", null), // FIXME description
				new CanonTag (CanonId.OwnerName, EntryType.Long, 32, "Owner Name", "Name of the Camera Owner"), // FIXME description
				new CanonTag (CanonId.Unknown4, EntryType.Short, -1, null, null),				
				new CanonTag (CanonId.CameraSerialNumber, EntryType.Short, 1, "Serial Number", null), //FIXME description
				new CanonTag (CanonId.Unknown4, EntryType.Short, -1, null, null),				
				new CanonTag (CanonId.CustomFunctions, EntryType.Short, -1, "Custom Functions", "Camera Custom Functions")
			};
					 
			foreach (CanonTag tag in tags)
				Tags [tag.Id] = tag;
		}

	}
	
	public enum Endian {
		Big,
		Little
	}

	public class ParseException : System.Exception 
	{
		public ParseException (string msg) : base (msg)
		{
		}
	}

	public class ShortReadException : ParseException 
	{
		public ShortReadException () : base ("Short Read")
		{
		}
	}

	public class Converter {
		public static uint ReadUInt (System.IO.Stream stream, Endian endian)
		{
			byte [] tmp = new byte [4];

		        if (stream.Read (tmp, 0, tmp.Length) < 4) {
#if DEBUG_LOADER
				System.Console.WriteLine ("short read XXXXXXXXXXXXXXXXXXXXXXx");
#endif
				throw new ShortReadException ();
			}
			return BitConverter.ToUInt32 (tmp, 0, endian == Endian.Little);
		}

		public static ushort ReadUShort (System.IO.Stream stream, Endian endian)
		{
			byte [] tmp = new byte [2];

		        if (stream.Read (tmp, 0, tmp.Length) < 2) {
#if DEBUG_LOADER
				System.Console.WriteLine ("Short read");
#endif
				throw new ShortReadException ();
			}

			return BitConverter.ToUInt16 (tmp, 0, endian == Endian.Little);
		}
	}

	public class Header : SemWeb.StatementSource {
		public Endian endian;

		private uint directory_offset;
		public ImageDirectory Directory;

                // false seems a safe default
                public bool Distinct {
                        get { return false; }
                }

		public Header (System.IO.Stream stream)
		{
			//using (new Timer ("new Tiff.Header")) {
			byte [] data = new byte [8];
			stream.Read (data, 0, data.Length);
			if (data [0] == 'M' && data [1] == 'M')
				endian = Endian.Big;
			else if (data [0] == 'I' && data [1] == 'I')
				endian = Endian.Little;

			ushort marker = BitConverter.ToUInt16 (data, 2, endian == Endian.Little);
			switch (marker) {
			case 42:
				//System.Console.WriteLine ("Found Standard Tiff Marker {0}", marker);
				break;
			case 0x4f52:
				//System.Console.WriteLine ("Found Olympus Tiff Marker {0}", marker.ToString ("x"));
				break;
			case 0x4e31:
				//System.Console.WriteLine ("Found Navy Interchange File Format Tiff Marker {0}", marker.ToString ("x")); 
				break;
			default:
				//System.Console.WriteLine ("Found Unknown Tiff Marker {0}", marker.ToString ("x"));
				break;
			}

			//System.Console.WriteLine ("Converting Something");
			directory_offset = BitConverter.ToUInt32 (data, 4, endian == Endian.Little);
			
			if (directory_offset < 8)
				throw new ParseException ("Invalid IFD0 Offset [" + directory_offset.ToString () + "]"); 
			
#if DEBUG_LOADER
			System.Console.WriteLine ("Reading First IFD");
#endif
			Directory = new ImageDirectory (stream, directory_offset, endian); 
			//}
		}
		
		
		public void Select (SemWeb.StatementSink sink)
		{
			//using (new Timer ("Tiff.Header.Select")) {
				SelectDirectory (Directory, sink);
			//}
		}

		public void SelectDirectory (ImageDirectory dir, StatementSink sink)
		{
			foreach (DirectoryEntry e in dir.Entries) {
#if DEBUG_LOADER
				System.Console.WriteLine ("{0}", e.Id);
#endif
				switch (e.Id) {
				case TagId.IPTCNAA:
					System.IO.Stream iptcstream = new System.IO.MemoryStream (e.RawData);
					FSpot.Iptc.IptcFile iptc = new FSpot.Iptc.IptcFile (iptcstream);
					iptc.Select (sink);
					break;
				case TagId.PhotoshopPrivate:
					System.IO.Stream bimstream = new System.IO.MemoryStream (e.RawData);
					FSpot.Bim.BimFile bim = new FSpot.Bim.BimFile (bimstream);
					bim.Select (sink);
					break;
				case TagId.XMP:
					System.IO.Stream xmpstream = new System.IO.MemoryStream (e.RawData);
					FSpot.Xmp.XmpFile xmp = new FSpot.Xmp.XmpFile (xmpstream);
					xmp.Select (sink);
					break;
				case TagId.ImageDescription:
					MetadataStore.AddLiteral (sink, "dc:description", "rdf:Alt", 
								  new SemWeb.Literal (e.ValueAsString [0], "x-default", null));
					break;
				case TagId.UserComment:
					MetadataStore.AddLiteral (sink, "exif:UserComment", "rdf:Alt", 
								  new SemWeb.Literal (e.ValueAsString [0], "x-default", null));
					break;
				case TagId.Copyright:
					MetadataStore.AddLiteral (sink, "dc:rights", "rdf:Alt", 
								  new SemWeb.Literal (e.ValueAsString [0], "x-default", null));
					break;
				case TagId.Artist:
					MetadataStore.Add (sink, "dc:creator", "rdf:Seq", e.ValueAsString);
					break;
				case TagId.ExifIfdPointer:
					try {
						ImageDirectory sub = ((SubdirectoryEntry)e).Directory [0];
						SelectDirectory (sub, sink);
					} catch (System.Exception exc) {
						//System.Console.WriteLine (exc);
					}
					break;
				case TagId.Software:
					MetadataStore.AddLiteral (sink, "xmp:CreatorTool", e.ValueAsString [0]);
					break;
				case TagId.DateTime:
					try {

					MetadataStore.AddLiteral (sink, "xmp:ModifyDate", 
								  e.ValueAsDate.ToString ("yyyy-MM-ddThh:mm:ss"));
					} catch (System.Exception ex) {
						//System.Console.WriteLine (String.Format ("error parsing {0}{2}{1}", e.ValueAsString[0], ex, Environment.NewLine));
					}

					break;
				case TagId.DateTimeOriginal:
				case TagId.DateTimeDigitized:
					// FIXME subsectime needs to be included in these values
					// FIXME shouldn't DateTimeOriginal be xmp:CreateDate? the spec says no but wtf?
					try {
						MetadataStore.AddLiteral (sink, "exif:" + e.Id.ToString (), 
									  e.ValueAsDate.ToString ("yyyy-MM-ddThh:mm:ss"));
					} catch (System.Exception ex) {
						//System.Console.WriteLine (String.Format ("error parsing {0}{2}{1}", e.ValueAsString[0], ex, Environment.NewLine));
					}
					break;
					//case TagId.SpatialFrequencyResponse
				case TagId.ExifCFAPattern:
					CFAPattern pattern = new CFAPattern (e.RawData, e.IsLittle);
					Entity empty = new BNode ();
					Statement top = new Statement (MetadataStore.FSpotXMPBase, 
								       (Entity)MetadataStore.Namespaces.Resolve ("exif:" + e.Id.ToString ()),
								       empty);
					
					Statement cols = new Statement (empty, 
									(Entity) MetadataStore.Namespaces.Resolve ("exif:Columns"),
									new SemWeb.Literal (pattern.Columns.ToString (), null, null));
					sink.Add (cols);
					Statement rows = new Statement (empty, 
									(Entity) MetadataStore.Namespaces.Resolve ("exif:Rows"),
									new SemWeb.Literal (pattern.Rows.ToString (), null, null));
					sink.Add (rows);
					string [] vals = e.ArrayToString (pattern.Values);
					MetadataStore.Add (sink, empty, "exif:Values", "rdf:Seq", vals);
					sink.Add (top);
					break;
				case TagId.ExifVersion:
				case TagId.FlashPixVersion:
				case TagId.ColorSpace:
				case TagId.CompressedBitsPerPixel:
				case TagId.PixelYDimension:
				case TagId.PixelXDimension:
				case TagId.RelatedSoundFile:
				case TagId.ExposureTime:
				case TagId.FNumber:
				case TagId.ExposureProgram:
				case TagId.SpectralSensitivity:
				case TagId.ShutterSpeedValue:
				case TagId.ApertureValue:
				case TagId.BrightnessValue:
				case TagId.ExposureBiasValue:
				case TagId.MaxApertureValue:
				case TagId.SubjectDistance:
				case TagId.MeteringMode:
				case TagId.LightSource:
				case TagId.FocalLength:
				case TagId.FlashEnergy:
				case TagId.FocalPlaneXResolution:
				case TagId.FocalPlaneYResolution:
				case TagId.FocalPlaneResolutionUnit:
				case TagId.ExposureIndex:
				case TagId.SensingMethod:
				case TagId.FileSource:
				case TagId.SceneType:
				case TagId.CustomRendered:
				case TagId.ExposureMode:
				case TagId.WhiteBalance:
				case TagId.DigitalZoomRatio:
				case TagId.FocalLengthIn35mmFilm:
				case TagId.SceneCaptureType:
				case TagId.GainControl:
				case TagId.Contrast:
				case TagId.Saturation:
				case TagId.Sharpness:
					MetadataStore.AddLiteral (sink, "exif:" + e.Id.ToString (), e.ValueAsString [0]);
					break;
				case TagId.ComponentsConfiguration:
				case TagId.ISOSpeedRatings:
				case TagId.SubjectArea:
				case TagId.SubjectLocation:
					MetadataStore.Add (sink, "exif:" + e.Id.ToString (), "rdf:Seq", e.ValueAsString);
					break;
				case TagId.TransferFunction:
				case TagId.YCbCrSubSampling:
				case TagId.WhitePoint:
				case TagId.PrimaryChromaticities:
				case TagId.YCbCrCoefficients:
				case TagId.ReferenceBlackWhite:
				case TagId.BitsPerSample:
					MetadataStore.Add (sink, "tiff:" + e.Id.ToString (), "rdf:Seq", e.ValueAsString);
					break;
				case TagId.Orientation:
				case TagId.Compression:
				case TagId.PhotometricInterpretation:					
				case TagId.SamplesPerPixel:
				case TagId.PlanarConfiguration:
				case TagId.YCbCrPositioning:
				case TagId.ResolutionUnit:
				case TagId.ImageWidth:
				case TagId.ImageLength:
				case TagId.Model:
				case TagId.Make:
					try {
						MetadataStore.AddLiteral (sink, "tiff:" + e.Id.ToString (), e.ValueAsString [0]);
					} catch (System.Exception ex) {
						//System.Console.WriteLine (String.Format ("error parsing {0}{2}{1}", e.Id, ex, Environment.NewLine));
					}
					break;
				}
			}
		}

		public void Save (System.IO.Stream out_stream)
		{
			OrderedWriter writer = new OrderedWriter (out_stream, endian == Endian.Little);
			
			/* Header */
			if (endian == Endian.Little) {
				writer.Write ((byte)'I');
				writer.Write ((byte)'I');
			} else {
				writer.Write ((byte)'M');
				writer.Write ((byte)'M');
			}
			
			writer.Write ((ushort)42);
			
			/* First IFD */
			Directory.Save (writer, 8);
		}

		public void Dump (string name)
		{
			ImageDirectory ifd = Directory;
			for (int i = 0; ifd != null; i++) {
				ifd.Dump (System.String.Format ("IFD[{0}]:", i));
				ifd = ifd.NextDirectory;
			}
		}
	}

	public class ImageDirectory {
		protected Endian endian;
		protected ushort num_entries;
		protected List<DirectoryEntry> entries;
		protected uint orig_position;

		protected uint next_directory_offset;
		ImageDirectory next_directory;
		
		protected bool has_header;
		protected bool has_footer;

		public ImageDirectory (System.IO.Stream stream, uint start_position, Endian endian)
		{
			this.endian = endian;
			orig_position = start_position;
			Load (stream);
		}
		
		public uint Save (OrderedWriter writer, uint position)
		{
			writer.Write (position);
			writer.Stream.Position = position;

			writer.Write ((ushort)entries.Count);

			position += 2;
			uint  value_position = (uint) (position + 12 * entries.Count + 4);

			for (int i = 0; i < entries.Count; i++) {
				writer.Stream.Position = position + (12 * i);
				value_position = (uint)Entries[i].Save (writer, value_position);
			}
							
			writer.Stream.Position = position + (12 * entries.Count);
			if (next_directory != null)
				value_position = next_directory.Save (writer, value_position);
			else 
				writer.Write ((uint) 0);

			return value_position;
		}

		protected void Load (System.IO.Stream stream)
		{
			ReadHeader (stream);			
			ReadEntries (stream);
			ReadFooter (stream);
			
			LoadEntries (stream);
			LoadNextDirectory (stream);
		}

		public virtual bool ReadHeader (System.IO.Stream stream)
		{
			stream.Seek ((long)orig_position, System.IO.SeekOrigin.Begin);
			return true;
		}

		protected virtual void ReadEntries (System.IO.Stream stream) 
		{
			num_entries = Converter.ReadUShort (stream, endian);
#if DEBUG_LOADER
			System.Console.WriteLine ("reading {0} entries", num_entries);
#endif			
			entries = new List<DirectoryEntry> (num_entries);
			int entry_length = num_entries * 12;
			byte [] content = new byte [entry_length];
			
			if (stream.Read (content, 0, content.Length) < content.Length) {
#if DEBUG_LOADER
				System.Console.WriteLine ("short read XXXXXXXXXXXXXXXXXXXXXXx");
#endif
				throw new ShortReadException ();
			}


			for (int pos = 0; pos < entry_length; pos += 12) {
				DirectoryEntry entry = CreateEntry (this, content, pos, this.endian);
				entries.Add (entry);		
#if DEBUG_LOADER
				System.Console.WriteLine ("Added Entry {0} {1} - {2} * {3}", entry.Id.ToString (), entry.Id.ToString ("x"), entry.Type, entry.Count);
#endif
				if (entry.Id == TagId.NewSubfileType) {
					
				}
			}
		}

		protected virtual void ReadFooter (System.IO.Stream stream)
		{
			next_directory_offset = Converter.ReadUInt (stream, this.endian);
		}

		protected void LoadEntries (System.IO.Stream stream)
		{
			foreach (DirectoryEntry entry in entries) {
				entry.LoadExternal (stream);
			}
		}
		
		protected void LoadNextDirectory (System.IO.Stream stream)
		{
#if DEBUG_LOADER
			System.Console.WriteLine ("start_position = {1} next_directory_offset = {0}",
						  next_directory_offset, orig_position);
#endif
			next_directory = null;
			try {
				if (next_directory_offset != 0 && next_directory_offset != orig_position)
					next_directory = new ImageDirectory (stream, next_directory_offset, this.endian);
				
			} catch (System.Exception) {
				//System.Console.WriteLine ("Error loading directory {0}", e.ToString ());
				next_directory = null;
				next_directory_offset = 0;
			}		
		}

		public ImageDirectory NextDirectory {
			get {
				return next_directory;
			}
		}

		public List<DirectoryEntry> Entries {
			get { 
				return entries;
			}
		}

		public DirectoryEntry Lookup (TagId id) 
		{
			foreach (DirectoryEntry entry in entries)
				if (entry.Id == id)
					return entry;
			

			return null;
		}

		private DirectoryEntry GetEntry (int i)
		{
			if (i < Entries.Count)
				return Entries [i];
			else
				return null;
		}

		public DirectoryEntry Lookup (uint id) 
		{
			foreach (DirectoryEntry entry in entries)
				if ((uint)entry.Id == id)
					return entry;

			return null;
		}

		public static DirectoryEntry CreateEntry (ImageDirectory parent, byte [] input, int start, Endian header_endian)
		{
			TagId tagid;
			EntryType type;

			DirectoryEntry.ParseHeader (input, start, out tagid, out type, header_endian);
			//ConstructorFunc ctor = ctors[tagid];			
			//if (ctor == null) {
			//	return ctor (input, header_endian);				
			//}
			
			switch (tagid) {
			case TagId.ExifIfdPointer:
			case TagId.GPSInfoIfdPointer:
			case TagId.InteroperabilityIfdPointer:
			case TagId.SubIFDs:
				return new SubdirectoryEntry (input, start, header_endian);
				//case TagId.MakerNote:
				//return new MakerNoteEntry (input, start, header_endian);
				//case TagId.PimIfdPointer:
				//return new 
				//case TagId.MakerNote:
				//return new MakerNoteEntry (input, start, header_endian);
			}
			
			switch (type) {
			case EntryType.Ifd:
				//System.Console.WriteLine ("Trying to load {0} {1}", tagid, tagid.ToString ("x"));
				return new SubdirectoryEntry (input, start, header_endian);
			case EntryType.Byte:
				return new ByteEntry (input, start, header_endian);
			case EntryType.Long:
				return new LongEntry (input, start, header_endian);
			case EntryType.Short:
				return new ShortEntry (input, start, header_endian);
			}

			return new DirectoryEntry (input, start, header_endian);
		}
		
#if false
		public Cms.Profile GetProfile ()
		{
			Cms.ColorCIExyY whitepoint = new Cms.ColorCIExyY (0, 0, 0);
			Cms.ColorCIExyYTriple primaries = new Cms.ColorCIExyYTriple (whitepoint, whitepoint, whitepoint);
			Cms.GammaTable [] transfer = null;
			int bits_per_sample = 8;
			double gamma = 2.2;
			
			foreach (DirectoryEntry e in entries) {
				switch (e.Id) {
				case TagId.InterColorProfile:
					try {
						return new Cms.Profile (e.RawData);
					} catch (System.Exception ex) {
						//System.Console.WriteLine (ex);
					}
					break;
				case TagId.ColorSpace:
					switch ((ColorSpace)e.ValueAsLong [0]) {
					case ColorSpace.StandardRGB:
						return Cms.Profile.CreateStandardRgb ();
					case ColorSpace.AdobeRGB:
						return Cms.Profile.CreateAlternateRgb ();
					case ColorSpace.Uncalibrated:
						//System.Console.WriteLine ("Uncalibrated colorspace");
						break;
					}
					break;
				case TagId.WhitePoint:
					Rational [] white = e.RationalValue;
					whitepoint.x = white [0].Value;
					whitepoint.y = white [1].Value;
					whitepoint.Y = 1.0;
					break;
				case TagId.PrimaryChromaticities:
					Rational [] colors = e.RationalValue;
					primaries.Red.x = colors [0].Value;
					primaries.Red.y = colors [1].Value;
					primaries.Red.Y = 1.0;

					primaries.Green.x = colors [2].Value;
					primaries.Green.y = colors [3].Value;
					primaries.Green.Y = 1.0;

					primaries.Blue.x = colors [4].Value;
					primaries.Blue.y = colors [5].Value;
					primaries.Blue.Y = 1.0;
					break;
				case TagId.TransferFunction:
					ushort [] trns = e.ShortValue;
					ushort gamma_count = (ushort) (1 << bits_per_sample);
					Cms.GammaTable [] tables = new Cms.GammaTable [3];
					//System.Console.WriteLine ("Parsing transfer function: count = {0}", trns.Length);

					// FIXME we should use the TransferRange here
					// FIXME we should use bits per sample here
					for (int c = 0; c < 3; c++) {
						tables [c] = new Cms.GammaTable (trns, c * gamma_count, gamma_count);
					}

					transfer = tables;
					break;
				case TagId.ExifIfdPointer:
					SubdirectoryEntry exif = (SubdirectoryEntry) e;
					DirectoryEntry ee = exif.Directory [0].Lookup ((int)TagId.Gamma);
					
					if (ee == null)
						break;

					Rational rgamma = ee.RationalValue [0];
					gamma = rgamma.Value;
					break;
				}
			}

			if (transfer == null) {
				Cms.GammaTable basic = new Cms.GammaTable (1 << bits_per_sample, gamma);
				transfer = new Cms.GammaTable [] { basic, basic, basic };
			}

			// if we didn't get a white point or primaries, give up
			if (whitepoint.Y != 1.0 || primaries.Red.Y != 1.0)
				return null;
				
			return new Cms.Profile (whitepoint, primaries, transfer);
		}
#endif

		public void Dump (string name) 
		{
			System.Console.WriteLine ("Starting {0}", name);
			foreach (DirectoryEntry e in this.Entries)
				e.Dump (name);
			System.Console.WriteLine ("Ending {0}", name);
		}
		
		public string Dump2 ()
		{
			System.Text.StringBuilder builder = new System.Text.StringBuilder ();
			builder.Append ("Dummping IFD");
			foreach (DirectoryEntry entry in entries) {
				builder.Append (entry.ToString ()+ Environment.NewLine);

				if (entry is SubdirectoryEntry)
					builder.Append ("Found SUBDIRECTORYENTRY" + Environment.NewLine);
			}
			
			if (next_directory != null) {
				builder.Append ("Dummping Next IFD");
				builder.Append (next_directory.Dump2 ());
			}

			return builder.ToString ();
		}
	}
	
	public class MakerNoteEntry : SubdirectoryEntry {
		public MakerNoteEntry (byte [] data, int offset, Endian endian) : base (data, offset, endian)
		{
		
		}

		public override uint GetEntryCount ()
		{
			return 1;
		}

		public override void LoadExternal (System.IO.Stream stream)
		{
		}
	}


	public class SubdirectoryEntry : DirectoryEntry {
		public uint directory_offset;
		public ImageDirectory [] Directory;
		
		public SubdirectoryEntry (byte [] data, int offset, Endian endian) : base (data, offset, endian)
		{
			if (this.GetEntryCount () > 1) {
				//System.Console.WriteLine ("Count is greater than 1 ({1}) on Subdirectory {0} interesting", tagid, count);
			}
		}

		public override uint Save (OrderedWriter writer, uint position)
		{
#if DEBUG_LOADER			
			Console.WriteLine ("writing entry {0} {1} {2} - value offset = {3}", Id, Type, Count, position);
#endif

			writer.Write ((ushort)Id);
			writer.Write ((ushort)Type);
			writer.Write ((uint)Count);


			if (Directory.Length == 1) {
				writer.Write ((uint)position);
				position = Directory [0].Save (writer, position);
			} else if (Directory.Length > 1) {
				writer.Write ((uint)position);
				uint value_position = (uint) (position + Directory.Length * 4);
				for (int i = 0; i < Directory.Length; i++) {
					writer.Stream.Position = position + i * 4;
					value_position = Directory [i].Save (writer, value_position);
				}
				return value_position;
			} else
				writer.Write ((uint) 0);

			return position;
		}

		public virtual uint GetEntryCount ()
		{
			return count;
		}

		public override void LoadExternal (System.IO.Stream stream)
		{
			uint entry_count = GetEntryCount ();
			Directory = new ImageDirectory [entry_count];

			base.LoadExternal (stream);

			for (int i = 0; i <  entry_count; i++) {
				try {
					directory_offset = BitConverter.ToUInt32 (raw_data, i * 4, endian == Endian.Little);
					if (directory_offset == offset_origin + 8)
						throw new Exception ("recursive ifd");
					Directory [i] = new ImageDirectory (stream, directory_offset, endian);
				} catch (System.Exception e) {
					//System.Console.WriteLine ("Error loading Subdirectory {0} at {2} of {3}bytes:{4}{1}", 
					//			  this.Id, e, directory_offset, stream.Length, Environment.NewLine);
				}
					
			}
		}

		public override void Dump (string name)
		{
			for (int i = 0; i < GetEntryCount (); i++) {
				string subdirname = System.String.Format ("{0}{1}[{2}]({3})]", name, tagid, i, directory_offset);

				try {
					if (Directory [i] != null)
						Directory [i].Dump (subdirname);
				} catch (System.Exception e) {
					System.Console.WriteLine (e);
				}
			}
		}
	}
	
	public class ShortEntry : DirectoryEntry {
		public ShortEntry (byte [] data, int offset, Endian endian) : base (data, offset, endian)
		{
		}
	}
	
	public class LongEntry : DirectoryEntry {
		public LongEntry (byte [] data, int offset, Endian endian) : base (data, offset, endian)
		{
			if (type != EntryType.Long)
				throw new ParseException (System.String.Format ("Invalid Settings At Birth {0}", tagid));
		}
	}

	public class ByteEntry : DirectoryEntry {
		public ByteEntry (byte [] data, int offset, Endian endian) : base (data, offset, endian)
		{
			if (type != EntryType.Byte)
				throw new ParseException ("Invalid Settings At Birth");
		}
	}
	
#if false
	public class ImageLoader {
		int width;
		int length;
		int [] bps;
		PhotometricInterpretation interpretation;
		Compression compression;
		uint [] offsets;
		uint [] strip_byte_counts;
		uint rows_per_strip;
		byte [] strip;

		public ImageLoader (ImageDirectory directory) 
		{
			width = directory.Lookup (TagId.ImageWidth).ValueAsLong [0];
			length = directory.Lookup (TagId.ImageLength).ValueAsLong [0];
			bps = directory.Lookup (TagId.BitsPerSample).ValueAsLong;
			
			compression = (Compression) directory.Lookup (TagId.Compression).ValueAsLong [0];
			interpretation = (PhotometricInterpretation) directory.Lookup (TagId.PhotometricInterpretation).ValueAsLong [0];
			
			offsets = directory.Lookup (TagId.StripOffsets).ValueAsLong;
			strip_byte_counts = directory.Lookup (TagId.StripByteCounts).ValueAsLong;
			rows_per_strip = directory.Lookup (TagId.RowsPerStrip).ValueAsLong [0];

			if (interpretation != 
		}


#if false
		public Gdk.Pixbuf LoadPixbuf (System.IO.Stream stream) 
		{
			Gdk.Pixbuf dest = new Gdk.Pixbuf (Gdk.Colorspace.Rgb, false, width, height);
			strip = new byte [strip_byte_counts];
			int row;
			for (int i = 0; i < offsets.Length; i++) {
				strip = new byte [strip_byte_counts [i]];
				stream.Read (strip, 0, strip.Length);
				switch (compression) {
					case Compression.Notice

				}
			}
		}
#endif
	}
#endif

	public class DirectoryEntry {
		protected TagId  tagid;
		protected EntryType type;
		protected uint count;
		protected uint offset_origin;
		protected uint data_offset;

		protected byte [] raw_data;
		protected Endian endian;

		public TagId Id {
			get {
				return tagid;
			}
		}

		public EntryType Type {
			get {
				return type;
			}
		}
		
		public uint Count {
			get {
				return count;
			}
		}

		public virtual uint Save (OrderedWriter writer, uint position)
		{
#if DEBUG_LOADER			
			Console.WriteLine ("writing entry {0} {1} {2}", Id, Type, Count);
#endif
			writer.Write ((ushort)Id);
			writer.Write ((ushort)Type);
			writer.Write ((uint)Count);
			if (Length > 4) {
				writer.Write ((uint)position);
				writer.Stream.Position = position;
				writer.Stream.Write (RawData, 0, RawData.Length);
				return (uint) (position + RawData.Length);
			} else {
				writer.Stream.Write (RawData, 0, RawData.Length);
				for (int i = 0; i < 4 - RawData.Length; i++)
					writer.Write ((byte)0);
			}
			return position;
		}

		public void SetOrigin (uint pos)
		{
			offset_origin = pos;
		}

		public uint Position {
			get {
				return offset_origin + data_offset;
			}
		}

		public uint Length {
			get { return (uint)(Count * GetTypeSize ()); }
		}

		public virtual int GetTypeSize ()
		{
			return GetTypeSize (type);
		}

		public static int GetTypeSize (EntryType type)
		{
			switch (type) {
			case EntryType.Byte:
			case EntryType.SByte:
			case EntryType.Undefined:
			case EntryType.Ascii:
				return 1;
			case EntryType.Short:
			case EntryType.SShort:
				return 2;
			case EntryType.Long:
			case EntryType.SLong:
			case EntryType.Float:
				return 4;
			case EntryType.Double:
			case EntryType.Rational:
			case EntryType.SRational:
				return 8;
			default:
				return 1;
			}
		}

		public bool IsLittle {
			get {
				return (endian == Endian.Little);
			}
		}

		public static int ParseHeader (byte [] data, int start, out TagId tagid, out EntryType type, Endian endian)
		{
			tagid = (TagId) BitConverter.ToUInt16 (data, start, endian == Endian.Little);
			type = (EntryType) BitConverter.ToUInt16 (data, start + 2, endian == Endian.Little);
			return 4;
		}
		
		public DirectoryEntry (byte [] data, int start, Endian endian)
		{
			this.endian = endian;

			start += ParseHeader (data, start, out this.tagid, out this.type, endian);
			ParseStream (data, start);
		}

		public virtual void LoadExternal (System.IO.Stream stream)
		{
			if (data_offset != 0) {
				stream.Seek ((long)Position, System.IO.SeekOrigin.Begin);
				byte [] data = new byte [count * GetTypeSize ()];
				if (stream.Read (data, 0, data.Length) < data.Length) {
#if DEBUG_LOADER
					System.Console.WriteLine ("Short read");
#endif
					throw new ShortReadException ();
				}
				raw_data = data;
			}

#if DEBUG_LOADER
			switch ((int)this.Id) {
			case (int)TagId.NewSubfileType:
				//System.Console.WriteLine ("XXXXXXXXXXXXXXXXXXXXX new NewSubFileType {0}", (NewSubfileType) this.ValueAsLong [0]);
				break;
			case (int)TagId.SubfileType:
				//System.Console.WriteLine ("XXXXXXXXXXXXXXXXXXXXX new SubFileType {0}", (SubfileType) this.ValueAsLong [0]);
				break;
			case (int)TagId.Compression:
				//System.Console.WriteLine ("XXXXXXXXXXXXXXXXXXXXX new Compression {0}", (Compression) this.ValueAsLong [0]);
				
				break;
			case (int)TagId.JPEGProc:
				//System.Console.WriteLine ("XXXXXXXXXXXXXXXXXXXXX new JPEGProc {0}", (JPEGProc) this.ValueAsLong [0]);
				
				break;
			case (int)TagId.PhotometricInterpretation:
				//System.Console.WriteLine ("XXXXXXXXXXXXXXXXXXXXX new PhotometricInterpretation {0}", (PhotometricInterpretation) this.ValueAsLong [0]);
				break;
			case (int)TagId.ImageWidth:
			case (int)TagId.ImageLength:
				//System.Console.WriteLine ("XXXXXXXXXXXXXXXXXXXXX new {1} {0}", this.ValueAsLong [0], this.Id);
				break;
			case 50648:
			case 50656:
			case 50752:
				//System.Console.WriteLine ("XXXXXXXXXXXXXXXXXXXXX {0}({1}) - {2} {3}", this.Id, this.Id.ToString ("x"), this.type, raw_data.Length);
				//System.Console.WriteLine ("XXXX ", System.Text.Encoding.ASCII.GetString (raw_data));
				switch (this.type) {
				case EntryType.Long:
					foreach (uint val in ((LongEntry)this).Value)
						System.Console.Write (" {0}", val);
					break;
				case EntryType.Short:
					foreach (ushort val in ((ShortEntry)this).ShortValue)
						System.Console.Write (" {0}", val);
					break;
				case EntryType.Byte:
					foreach (byte val in this.RawData)
						System.Console.Write (" {0}", val);
					break;
				}
				//System.Console.WriteLine (String.Empty);
				break;
			}
#endif
		}

		public virtual void Dump (string name)
		{
			switch (this.Type) {
			case EntryType.Short:
			case EntryType.Long:
				uint [] vals = this.ValueAsLong;
				System.Console.Write ("{3}{1}({2}) [{0}] (", vals.Length, this.Id, this.Type, name);
				for (int i = 0; i < System.Math.Min (15, vals.Length); i++) {
					System.Console.Write (" {0}", vals [i]);
				}
				System.Console.WriteLine (")");
				break;
			case EntryType.Ascii:
				System.Console.WriteLine ("{3}{1}({2}) (\"{0}\")", this.StringValue, this.Id, this.Type, name);
				break;
			default:
				System.Console.WriteLine ("{3}{1}({2}) [{0}]", this.Count, this.Id, this.Type, name);
				break;
			}
		}
		
		protected void ParseStream (byte [] data, int start)
		{
			int i = start;

			count = BitConverter.ToUInt32 (data, i, endian == Endian.Little); 
			i += 4;
			int size = (int)count * GetTypeSize ();
			if (size > 4)
				data_offset = BitConverter.ToUInt32 (data, i, endian == Endian.Little);
			else {
				data_offset = 0;
				raw_data = new byte [size];
				System.Array.Copy (data, i, raw_data, 0, size);
			}
		}

		public void SetData (string value)
		{
			int len = System.Text.Encoding.UTF8.GetByteCount (value);
			byte [] tmp = new byte [len + 1];
			System.Text.Encoding.UTF8.GetBytes (value, 0, value.Length, tmp, 0);
			tmp[len] = 0;
			//System.Console.WriteLine ("SetData: value = {0} len = {1}", value, len);
			SetData (tmp);
		}
	
		public static System.DateTime DateTimeFromString (string dt)
		{
			// Exif DateTime strings are formatted as
			//      "YYYY:MM:DD HH:MM:SS"
			
			string delimiters = " :";
			string[] dt_data = dt.Split ( delimiters.ToCharArray(), 6 );
			System.DateTime result;
			result = new System.DateTime (System.Int32.Parse(dt_data[0]), 
						      System.Int32.Parse(dt_data[1]), 
						      System.Int32.Parse(dt_data[2]),
						      System.Int32.Parse(dt_data[3]), 
						      System.Int32.Parse(dt_data[4]), 
						      System.Int32.Parse(dt_data[5]));
			
			return result;
		}
		
		public void SetData (byte [] data)
		{
			raw_data = data;
			count = (uint)raw_data.Length / (uint)GetTypeSize ();
		}

#if false		
		public object  GetValue () {
			switch (Type) {
			case EntryType.Short:
				return ShortValue;
			case EntryType.Long:
				return LongValue;
			case  EntryType.Rational:
				return RationalValue;
			case EntryType.SRational:
				return SRationalValue;
			case EntryType.Ascii:
				return StringValue.Split ('\0');
				break;
			default:
				//System.Console.WriteLine ("{1}({2}) [{0}]", this.Count, this.Id, this.Type);
				break;

				}
			}
		}
#endif

		public byte [] Value {
			get {
				return raw_data;
			}
		}

		public byte [] RawData {
			get { 
				return raw_data;
			}
		}

		public string [] ValueAsString {
			get {
				switch (this.Type) {
				case EntryType.Short:
				case EntryType.Long:
					return ArrayToString (this.ValueAsLong);
				case EntryType.Rational:
					return ArrayToString (this.RationalValue);
				case EntryType.SRational:
					return ArrayToString (this.SRationalValue);
				case EntryType.Undefined:
					switch (Id) {
					case TagId.UserComment:
						return new string [] { UserCommentValue };
					case TagId.FlashPixVersion:
					case TagId.ExifVersion:
						return new string [] { StringValue };
					case TagId.FileSource:
					case TagId.SceneType:
						return ArrayToString (this.RawData);
					case TagId.ComponentsConfiguration:
						return ArrayToString (ValueAsLong);
					default:
						//System.Console.WriteLine ("Cannot convert type \"{0}\" to string", Id);
						break;
					}
					break;
				case EntryType.Ascii:
					return StringValue.Split ('\0');
				}
				return null;
			}
		}

		public string [] ArrayToString (System.Array array)
		{
			string [] vals = new string [array.Length];
			for (int i = 0; i < array.Length; i++)
				vals [i] = array.GetValue (i).ToString ();

			return vals;
		}

		public uint [] ValueAsLong {
			get {
				uint [] data = new uint [this.Count];
				for (int i = 0; i < this.Count; i++) {
					switch (this.Type) {
					case EntryType.Long:
						data [i] = BitConverter.ToUInt32 (raw_data, i * GetTypeSize (), endian == Endian.Little);
						break;
					case EntryType.Short:
						data [i] = BitConverter.ToUInt16 (raw_data, i * GetTypeSize (), endian == Endian.Little);
						break;
					case EntryType.Undefined:
					case EntryType.Byte:
						data [i] = raw_data [i];
						break;
					default:
						throw new ParseException ("Invalid conversion");
					}
				}
				return data;
			}
		}
		
		// The following methods are usded to convert the data 
		// to the various type regardless of the entry
		// type, they are used internally in processing the data
		// Use at your own risk.

		public string StringValue {
			get {
				return System.Text.Encoding.ASCII.GetString (raw_data);
			}
		}

		public System.DateTime ValueAsDate {
			get {
				return DirectoryEntry.DateTimeFromString (StringValue);
			}
		}

		public string UserCommentValue {
			get {
				UserComment comment = new UserComment (raw_data, IsLittle);
				return comment.Value;
			}
		}

		public SRational [] SRationalValue {
			get {
				Rational [] vals = RationalValue;
				SRational [] data = new SRational [vals.Length];
				
				for (int i = 0; i < vals.Length; i++)
					data [i] = SRational.BitwiseCopy (vals [i]);

				return data;
			}
		}

		public Rational [] RationalValue {
			get {
				uint [] vals = LongValue;
				Rational [] data = new Rational [vals.Length / 2];

				for (int i = 0; i < vals.Length; i += 2)
					data [i/2] = new Rational (vals [i], vals [i + 1]);
				
				return data;
			}
			
		}

		public uint [] LongValue {
			get {
				uint [] data = new uint [raw_data.Length / 4];
				for (int i = 0; i < raw_data.Length; i+= 4)
					data [i/4] = BitConverter.ToUInt32 (raw_data, i, endian == Endian.Little);

				return data;
			}
		}

		public ushort [] ShortValue {
			get {
				ushort [] data = new ushort [raw_data.Length];
				for (int i = 0; i < raw_data.Length; i+= 2) {
					data [i] = BitConverter.ToUInt16 (raw_data, i, endian == Endian.Little);
				}
				return data;
			}
		}
	}


	public class TiffFile : ImageFile, SemWeb.StatementSource {
		public Header Header;

                // false seems a safe default
                public bool Distinct {
                        get { return false; }
                }

		public TiffFile (string path) : base (path)
		{
			try {
				using (System.IO.Stream input = Open ()) {
					this.Header = new Header (input);
				}

#if DEBUG_LOADER
				Header.Dump (this.ToString () + ":");
#endif
			} catch (System.Exception e) {
				Beagle.Util.Log.Error (e, "Error loading TIFF file {0}", path);
			}
		}

		public TiffFile (Uri uri) : base (uri)
		{
			try {
				using (System.IO.Stream input = Open ()) {
					this.Header = new Header (input);
				}

#if DEBUG_LOADER
				Header.Dump (this.ToString () + ":");
#endif
			} catch (System.Exception e) {
				Beagle.Util.Log.Error (e, "Error loading TIFF file {0}", uri);
			}
		}

		public virtual void Select (SemWeb.StatementSink sink)
		{
			Header.SelectDirectory (Header.Directory, sink);
		}

		public override System.DateTime Date {
			get {
				SubdirectoryEntry sub = (SubdirectoryEntry) this.Header.Directory.Lookup (TagId.ExifIfdPointer);
				DirectoryEntry e;

				if (sub != null) {
					e = sub.Directory [0].Lookup (TagId.DateTimeOriginal);
					
					if (e != null)
						return DirectoryEntry.DateTimeFromString (e.StringValue).ToUniversalTime ();
				}

				e = this.Header.Directory.Lookup (TagId.DateTime);

				if (e != null)
					return DirectoryEntry.DateTimeFromString (e.StringValue).ToUniversalTime ();
				else
					return base.Date;
			}
		}
		
		public override System.IO.Stream PixbufStream ()
		{
			return Open ();
		}

		public override PixbufOrientation GetOrientation ()
		{
			ShortEntry e = (ShortEntry)(this.Header.Directory.Lookup (TagId.Orientation));
			if (e != null) 
				return (PixbufOrientation)(e.ShortValue[0]);
			else
				return PixbufOrientation.TopLeft;
		}

		public System.IO.Stream LookupJpegSubstream (ImageDirectory directory)
		{
			uint offset = directory.Lookup (TagId.JPEGInterchangeFormat).ValueAsLong [0];
			
			System.IO.Stream file = Open ();
			file.Position = offset;
			return file;
		}

#if false
		public Gdk.Pixbuf LoadJpegInterchangeFormat (ImageDirectory directory)
		{
			uint offset = directory.Lookup (TagId.JPEGInterchangeFormat).ValueAsLong [0];
			uint length = directory.Lookup (TagId.JPEGInterchangeFormatLength).ValueAsLong [0];
			   
			using (System.IO.Stream file = Open ()) {
				file.Position = offset;
				
				byte [] data = new byte [32768];
				int read;

				Gdk.PixbufLoader loader = new Gdk.PixbufLoader ();
				
				while (length > 0) {
					read = file.Read (data, 0, (int)System.Math.Min ((int)data.Length, length));
					if (read <= 0)
						break;

					loader.Write (data, (ulong)read);
					length -= (uint) read;
				}
				Gdk.Pixbuf result = loader.Pixbuf;
				loader.Close ();
				return result; 
			}
		}
#endif
	}

	public class DngFile : TiffFile {
		public DngFile (string path) : base (path) 
		{
		}

		public DngFile (System.Uri uri) : base (uri) 
		{
		}

		public override System.IO.Stream PixbufStream ()
		{
			try {
				SubdirectoryEntry sub = (SubdirectoryEntry) Header.Directory.Lookup (TagId.SubIFDs);
				ImageDirectory directory = sub.Directory [sub.Directory.Length - 1];

				uint offset = directory.Lookup (TagId.StripOffsets).ValueAsLong [0];
				System.IO.Stream file = Open ();
				file.Position = offset;
				return file;
			} catch {
				return DCRawFile.RawPixbufStream (uri);
			}
		}

		public override void Select (SemWeb.StatementSink sink)
		{
			
			/* this is just a sanity pass, if the first ifd is not a subfile use the normal
			 * tiff path 
			 */
			DirectoryEntry e = Header.Directory.Lookup (TagId.NewSubfileType);
			if (e == null) {
				base.Select (sink);
				return;
			}

			/*
			 * Even though Ifd0 doesn't have the full resolution image
			 * it would have the XMP data so we look for it
			 */
			e = Header.Directory.Lookup (TagId.XMP);
			if (e != null) {
				System.IO.Stream xmpstream = new System.IO.MemoryStream (e.RawData);
				FSpot.Xmp.XmpFile xmp = new FSpot.Xmp.XmpFile (xmpstream);
				xmp.Select (sink);
			}

			/* 
			 * Ifd0 will also have the exif directory
			 */
			ImageDirectory dir = Header.Directory;
			SubdirectoryEntry sub = (SubdirectoryEntry) dir.Lookup (TagId.ExifIfdPointer);
			if (sub != null)
				Header.SelectDirectory (sub.Directory [0], sink);
			
			/*
			 * now we lookup subifd0 (we should probably scan the newsubfile types here)
			 * and load the metadata we are interested in from it.
			 */
			sub = (SubdirectoryEntry) Header.Directory.Lookup (TagId.SubIFDs);	

			if (e == null)
				e = dir.Lookup (TagId.NewSubfileType);

			int i = 0;
			do {
				uint dirtype = e.ValueAsLong [0];
				if (dirtype == 0) {
					Header.SelectDirectory (dir, sink);
					break;
				}
					
				if (sub == null)
					break;

				dir = sub.Directory [i];
				e = dir.Lookup (TagId.NewSubfileType);
				i++;
			} while (i < sub.Directory.Length);

			
		}
	}	
	
	public class NefFile : TiffFile, IThumbnailContainer {
		public NefFile (string path) : base (path) 
		{
		}

		public NefFile (Uri uri) : base (uri)
		{
		}

		public override void Select (SemWeb.StatementSink sink)
		{
			DirectoryEntry e = Header.Directory.Lookup (TagId.NewSubfileType);

			if (e == null) {
				base.Select (sink);
				return;
			}

			ImageDirectory dir = Header.Directory;
			SubdirectoryEntry sub = (SubdirectoryEntry) dir.Lookup (TagId.ExifIfdPointer);
			
			if (sub != null)
				Header.SelectDirectory (sub.Directory [0], sink);
			
			sub = (SubdirectoryEntry) Header.Directory.Lookup (TagId.SubIFDs);	

			int i = 0;
			do {
				uint dirtype = e.ValueAsLong [0];
				if (dirtype == 0) {
					Header.SelectDirectory (dir, sink);
					break;
				}
					
				if (sub == null)
					break;

				dir = sub.Directory [i];
				if (dir != null)
					e = dir.Lookup (TagId.NewSubfileType);
				i++;
			} while (i < sub.Directory.Length);
		}

#if false
		public Gdk.Pixbuf GetEmbeddedThumbnail ()
		{
			using (System.IO.Stream stream = Open ()) {
				return TransformAndDispose (new Gdk.Pixbuf (stream));
			}
		}

		public override System.IO.Stream PixbufStream ()
		{
			try {
				SubdirectoryEntry sub = (SubdirectoryEntry) Header.Directory.Lookup (TagId.SubIFDs);
				ImageDirectory jpeg_directory = sub.Directory [0];
				return LookupJpegSubstream (jpeg_directory);
			} catch (System.Exception) {
				return DCRawFile.RawPixbufStream (uri);
			}
		}
#endif
	}
		

	public class Cr2File : TiffFile, IThumbnailContainer {
		public Cr2File (string path) : base (path) 
		{
		}

		public Cr2File (Uri uri) : base (uri)
		{
		}

		/*
		public override PixbufOrientation GetOrientation ()
		{
			return PixbufOrientation.TopLeft;
		}
		*/

#if false
		public Gdk.Pixbuf GetEmbeddedThumbnail ()
		{
			ImageDirectory directory;
			directory = Header.Directory.NextDirectory;
			return TransformAndDispose (LoadJpegInterchangeFormat (directory));
		}
#endif

		public override System.IO.Stream PixbufStream ()
		{
			uint offset = Header.Directory.Lookup (TagId.StripOffsets).ValueAsLong [0];
			System.IO.Stream file = Open ();
			file.Position = offset;
			return file;
		}
	}

#if ENABLE_NUNIT
	[TestFixture]
	public class Tests {
		public Tests ()
		{
			Gnome.Vfs.Vfs.Initialize ();
			Gtk.Application.Init ();
		}
		
		[Test]
		public void Save ()
		{
			string desc = "this is an example description";
			string desc2 = "\x00a9 Novell Inc.";
			PixbufOrientation orient = PixbufOrientation.TopRight;
			Gdk.Pixbuf test = new Gdk.Pixbuf (null, "f-spot-32.png");
			string path = ImageFile.TempPath ("joe.jpg");
			
			PixbufUtils.SaveJpeg (test, path, 75, new Exif.ExifData ());
			JpegFile jimg = new JpegFile (path);
			jimg.SetDescription (desc);
			jimg.SetOrientation (orient);
			jimg.SaveMetaData (path);
			JpegFile mod = new JpegFile (path);
			Assert.AreEqual (mod.Orientation, orient);
			Assert.AreEqual (mod.Description, desc);
			jimg.SetDescription (desc2);
			jimg.SaveMetaData (path);
			mod = new JpegFile (path);
			Assert.AreEqual (mod.Description, desc2);
			
			Header header = mod.ExifHeader;
#if USE_TEST_FILE
			string tmp = "/home/lewing/test.tiff";
			if (File.Exists (tmp))
				File.Delete (tmp);
			Stream stream = File.Open (tmp, FileMode.Create, FileAccess.ReadWrite);
			Console.WriteLine ("XXXX saving tiff {0}", tmp);
#else
			System.IO.MemoryStream stream = new System.IO.MemoryStream ();
#endif

			header.Dump ("source");
			header.Save (stream);
			stream.Position = 0;
			//System.Console.WriteLine ("----------------------------------------------LOADING TIFF");
			Header loader = new Header (stream);
			loader.Dump ("loader");
			
			CompareDirectories (header.Directory, loader.Directory);

			System.IO.File.Delete (path);	
		}

		private void CompareDirectories (ImageDirectory olddir, ImageDirectory newdir)
		{
			Assert.AreEqual (olddir.Entries.Count, newdir.Entries.Count);
			for (int i = 0; i < olddir.Entries.Count; i++) {
				Assert.AreEqual (olddir.Entries [i].Id, newdir.Entries [i].Id);
				Assert.AreEqual (olddir.Entries [i].Type, newdir.Entries [i].Type);
				Assert.AreEqual (olddir.Entries [i].Count, newdir.Entries [i].Count);
				Assert.AreEqual (olddir.Entries [i].Length, newdir.Entries [i].Length);
				if (olddir.Entries [i] is SubdirectoryEntry) {
					SubdirectoryEntry oldsub = olddir.Entries [i] as SubdirectoryEntry;
					SubdirectoryEntry newsub = newdir.Entries [i] as SubdirectoryEntry;
					
					for (int j = 0; j < oldsub.Directory.Length; j++)
						CompareDirectories (oldsub.Directory [j], newsub.Directory [j]);
				}
			}
		}
	}
#endif
}

