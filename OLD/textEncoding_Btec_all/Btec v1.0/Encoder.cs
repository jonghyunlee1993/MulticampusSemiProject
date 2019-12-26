using System;
using TextConverter;
using System.IO;

namespace Btec
{
	/// <summary>
	/// Encoder�� ���� ��� �����Դϴ�.
	/// </summary>
	public class Encoder
	{
		public static System.Text.Encoding[] encodings
			= {System.Text.Encoding.ASCII, 
				  System.Text.Encoding.BigEndianUnicode,
				  System.Text.Encoding.Default,
				  System.Text.Encoding.Unicode,
				  System.Text.Encoding.UTF7,
				  System.Text.Encoding.UTF8,
				  System.Text.Encoding.GetEncoding(949),
				  System.Text.Encoding.GetEncoding(932)
			  };
		public static string[] encodingNames
			= {"ASCII","BigEndianUnicode","Default","Unicode (UTF-16)","UTF-7","UTF-8",
				"ks_c_5601-1987 (Korean)","iso-2022-jp (Shift-JIS, Japanese)"};

		public static int[] encodingCodePages;

		static Encoder()
		{
				encodingCodePages = new Int32[encodings.Length];
			for(int i=0;i<encodings.Length;i++)
				encodingCodePages[i] = encodings[i].CodePage;
		}
		public static string EncodingToString(int codePage)
		{
			for(int i=0;i<encodingCodePages.Length;i++)
			{
				if(codePage == encodingCodePages[i])
					return encodingNames[i];
			}
			return "";
		}
		public static string EncodingToString(System.Text.Encoding encoding)
		{
			for(int i=0;i<encodings.Length;i++)
			{
				if(encoding == encodings[i])
					return encodingNames[i];
			}
			return "";
		}

		public static string Convert(string path, System.Text.Encoding encoding, bool preserveDate, bool preserveAtri, bool simulaton)
		{
			string content;
			FileAttributes fa;
			DateTime ctu;
			DateTime lwtu;

			//���Ͽ���
			try
			{
				content = TextEncoding.ReadTextFile(path);
				fa = File.GetAttributes(path);
				ctu= File.GetCreationTimeUtc(path);
				lwtu= File.GetLastWriteTimeUtc(path);
			}
			catch(Exception)
			{
				return String.Format("������ ���� �����ϴ�.");
			}

			//��������
			try
			{
				
				if(!simulaton)
				{
					StreamWriter sw = new StreamWriter(path,false,encoding);
					sw.Write(content);
					sw.Flush();
					sw.Close();
				}
			}
			catch(Exception)
			{
				return String.Format("������ �����Ҽ� �����ϴ�.");
			}
			
			//�Ӽ� ����
			if(preserveDate)
			{
				File.SetCreationTimeUtc(path,ctu);
				File.SetLastWriteTimeUtc(path,lwtu);
			}
			if(preserveAtri)
				File.SetAttributes(path,fa);

			return "";
		}




	}
}
