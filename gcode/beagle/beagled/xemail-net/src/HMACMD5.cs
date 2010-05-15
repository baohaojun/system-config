using System;
using System.Security.Cryptography;

namespace System.Security.Cryptography
{
	/// <summary>
	/// 
	/// </summary>
	public class HMACMD5 : KeyedHashAlgorithm 
	{
		private MD5     hash1    = null;
		private MD5     hash2    = null;
		private bool    bHashing = false;
		private byte[]  rgbInner = new byte[64];
		private byte[]  rgbOuter = new byte[64];

		/// <summary>
		/// Rfc 2104.
		/// </summary>
		/// <param name="rgbKey"></param>
		public HMACMD5 (byte[] rgbKey) 
		{
			HashSizeValue = 128;
			// Create the hash algorithms.
			hash1 = MD5.Create();
			hash2 = MD5.Create();

			this.Key = rgbKey;
		}    

		/// <summary>
		/// 
		/// </summary>
		public override byte[] Key 
		{
			get { return (byte[])KeyValue.Clone(); }
			set 
			{
				if(bHashing){
					throw new Exception("Cannot change key during hash operation");
				}
				if(value.Length > 64){
					KeyValue = hash1.ComputeHash(value);
					// No need to call Initialize, ComputeHash does it automatically.
				}
				else{
					KeyValue = (byte[]) value.Clone();
				}
				// Compute rgbInner and rgbOuter.
				int i = 0;
				for(i=0;i<64;i++){ 
					rgbInner[i] = 0x36;
					rgbOuter[i] = 0x5C;
				}
				for(i=0;i<KeyValue.Length;i++){
					rgbInner[i] ^= KeyValue[i];
					rgbOuter[i] ^= KeyValue[i];
				}
			}
		}

		/// <summary>
		/// 
		/// </summary>
		public override void Initialize() 
		{
			hash1.Initialize();
			hash2.Initialize();
			bHashing = false;
		}

		/// <summary>
		/// 
		/// </summary>
		/// <param name="rgb"></param>
		/// <param name="ib"></param>
		/// <param name="cb"></param>
		protected override void HashCore(byte[] rgb,int ib,int cb) 
		{
			if(bHashing == false){
				hash1.TransformBlock(rgbInner,0,64,rgbInner, 0);
				bHashing = true;                
			}
			hash1.TransformBlock(rgb,ib,cb,rgb,ib);
		}

		/// <summary>
		/// 
		/// </summary>
		/// <returns></returns>
		protected override byte[] HashFinal() 
		{
			if(bHashing == false){
				hash1.TransformBlock(rgbInner,0,64,rgbInner,0);
				bHashing = true;                
			}
			// Finalize the original hash.
			hash1.TransformFinalBlock(new byte[0],0,0);
			// Write the outer array.
			hash2.TransformBlock(rgbOuter,0,64,rgbOuter, 0);
			// Write the inner hash and finalize the hash.
			hash2.TransformFinalBlock(hash1.Hash,0,hash1.Hash.Length);
			bHashing = false;
			return hash2.Hash;
		} 
	}
}
