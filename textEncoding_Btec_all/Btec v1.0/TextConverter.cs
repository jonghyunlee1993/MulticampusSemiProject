using System;
using System.IO;
using System.Collections;

namespace TextConverter
{
	/// <summary>
	/// ������ �˻���. (static �޼���)
	/// </summary>
	public class FileClass
	{
		
		private static ArrayList Folders = new ArrayList();//���� ����Ʈ
		//���� ���� ���θ� Ȯ��
		public static bool IsFileExist(string path)
		{
			return File.Exists(path);
		}
		public static bool IsFileExist(string[] paths)
		{
			foreach(string path in paths)
			{
				if(!(IsFileExist(path)))
					return false;
			}
			return true;
		}
		//���� ���� ���θ� Ȯ��, ������ ���ϸ��� ��ȯ
		public static bool IsFileExist(string[] paths, ref string name)
		{
			foreach(string path in paths)
			{
				if(!(IsFileExist(path)))
				{
					name = path;
					return false;
				}
			}
			return true;	
		}
		//��� ���� �������� ArrayList�� ����ϴ�.
		public static ArrayList GetSubFolders(string path)
		{
			Folders.Clear();
			AddSubFolders(path);
			Folders.RemoveAt(0);
			return Folders;
		}
		private static void AddSubFolders(string path)
		{
			Folders.Add(path);
			DirectoryInfo di = new DirectoryInfo(path);
			foreach(DirectoryInfo di2 in di.GetDirectories("*.*"))
			{
				AddSubFolders(di2.FullName);
			}
		}
		//���丮������ ���θ� �˻��մϴ�.
		public static bool IsDirName(string path)
		{
			if(Directory.Exists(path))
				return true;
			return false;
		}
	}


	/// <summary>
	/// ������ �ؽ�Ʈ ���ڵ��� ��ȯ, �����.
	/// </summary>
	public class Converter
	{
		private string[] filenames; //���ϸ���� ������ string�迭
		private System.Text.Encoding encoTo = System.Text.Encoding.UTF8; //��ȯ�� ���ڵ�, �⺻�� UTF8
		private System.Text.Encoding encoNow; //���� ������ ���ڵ�
		private string fileContent; //���� ���� ����

		//��ȯ�� ���� ����Ʈ ����
		public void SetFilename(string[] filenames)
		{
			this.filenames=filenames;
		}
		public void SetFilename(string filename)
		{
			this.filenames = new string[0];
			this.filenames[0]=filename;
		}

		//��ȯ�� ���ڵ� ����
		public void SetEncoding(System.Text.Encoding enco)
		{
			this.encoTo=enco;
		}
		public void SetEncoding(int enco)
		{
			this.encoTo=System.Text.Encoding.GetEncoding(enco);
		}
		public void SetEncoding(string enco)
		{
			this.encoTo=System.Text.Encoding.GetEncoding(enco);
		}

		//filenames�迭�� �ִ� ������ ��ȯ������
		public void StartConvert()
		{
			Print("/'"+TextEncoding.EncodingToString(encoTo)+"/'���ڵ����� ��ȯ�� �����մϴ�.\n\n");
			for(int i=0;i<filenames.Length;i++)
			{
				Print("["+i.ToString()+"] "+filenames[i]+"\n");
				this.Convert(filenames[i]);
			}
		}

		//���� ��ȯ
		public void Convert(string filename)
		{
			try
			{
				//���ڵ� �� ��������
				this.encoNow=TextEncoding.Get(filename);
				if(encoNow==encoTo)
				{
					Print(" �̹� /'"+TextEncoding.EncodingToString(encoNow)+"/'�̹Ƿ� �����մϴ�.\n");
				}
				else
				{
					//�ҷ�����
					LoadFile(filename);
					//�����ϱ�
					SaveFile(filename);
					Print(" /'"+TextEncoding.EncodingToString(encoNow)+"/'���� /'"+TextEncoding.EncodingToString(encoTo)+"/'���� ��ȯ �Ǿ����ϴ�.\n");
				}
			}
			catch(Exception e)
			{
				Print(" ����: "+e.Message);
			}
		}
		public void Convert(string filename, System.Text.Encoding enco)
		{
			this.encoTo = enco;
			this.Convert(filename);
		}

		//���ڵ� üũ ���
		public void Check(string filename)
		{
			try
			{
				//���ڵ� �� ��������
				this.encoNow=TextEncoding.Get(filename);
				Print("/'"+TextEncoding.EncodingToString(encoNow)+"/' ���ڵ� �Դϴ�.\n");
			}
			catch(Exception e)
			{
				Print(" ����: "+e.Message);
			}
		}

		//���� ��Ȳ ���
		protected virtual void Print(string message)
		{
			Console.Write(message);
		}

		//���� ����
		protected virtual void LoadFile(string filename)
		{
			StreamReader sr = new StreamReader(filename,encoNow);
			this.fileContent=sr.ReadToEnd();
			sr.Close();
		}

		//���� �����ϱ�
		protected virtual void SaveFile(string filename)
		{
			StreamWriter sw = new StreamWriter(filename,false,encoTo);
			sw.Write(fileContent);
			sw.Flush();
			sw.Close();
		}


	}


	/// <summary>
	/// TextConverter.TextEnoding Ŭ����
	/// Ư�� �ؽ�Ʈ ������ ���ڵ��� �˾� ���ϴ�.
	/// </summary>
	public class TextEncoding
	{
		private const int readLength = 5; //�о���� ����Ʈ ����
		private static byte[] fileContent = new byte[readLength]; //�о���� ����
		private static System.Text.Encoding defaultEncoding = System.Text.Encoding.Default; //���ڵ� �⺻��.

		/// <summary>
		/// �ؽ�Ʈ���� ���� ���ڵ��� �˻��մϴ�.
		/// </summary>
		/// <param name="filename">�˻��� �ؽ�Ʈ ���ϸ�</param>
		/// <returns>�ؽ�Ʈ ���ڵ�</returns>
		public static System.Text.Encoding Get(string filename)
		{
			try
			{
				//���� �б�
				FileStream fs = new FileStream(filename,FileMode.Open,FileAccess.Read);
				fs.Seek(0,SeekOrigin.Begin);
				for(int i=0;i<readLength;i++)
				{
					int temp;
					temp=fs.ReadByte();
					if (temp==-1)
						temp=32;
					fileContent[i]=(byte)temp;
				}
				fs.Close();

				//BigEndianUnicode
				if(fileContent[0]==0xFE && fileContent[1]==0xFF)
					return System.Text.Encoding.BigEndianUnicode;

				//Unicode
				if(fileContent[0]==0xFF && fileContent[1]==0xFE)
					return System.Text.Encoding.Unicode;

				//UTF8
				if(fileContent[0]==0xEF && fileContent[1]==0xBB && fileContent[2]==0xBF)
					return System.Text.Encoding.UTF8;

				//ASCII
				//UTF7
				//Default
				return defaultEncoding;
				
			}
			catch(FileNotFoundException e)
			{
				throw new Exception("������ ã���� �����ϴ�.",e);
			}
			catch(Exception e)
			{
				throw new Exception("������ ������ �����ϴ�.",e);
			}
		}
		
		//�迭�� ��밡�� : ���� ��ȯ �Ұ�
		public static System.Text.Encoding[] Get(string[] filename)
		{
			System.Text.Encoding[] enco = new System.Text.Encoding[filename.Length];
			for(int count=0;count<filename.Length;count++)
				enco[count] = Get(filename[count]);	
			return enco;
		}

		/// <summary>
		/// ���ڵ� ��Ī�� ����մϴ�.
		/// </summary>
		/// <param name="enco">����� ���ڵ�</param></param>
		/// <returns>���ڵ� ��Ī</returns>
		public static string EncodingToString(System.Text.Encoding enco)
		{
			if (enco==System.Text.Encoding.ASCII)
				return "ASCII";
			if (enco==System.Text.Encoding.BigEndianUnicode )
				return "BigEndianUnicode";
			if (enco==System.Text.Encoding.Default )
				return "Default";
			if (enco==System.Text.Encoding.Unicode )
				return "Unicode";
			if (enco==System.Text.Encoding.UTF7 )
				return "UTF-7";
			if (enco==System.Text.Encoding.UTF8 )
				return "UTF-8";
			return enco.CodePage.ToString();
		}
		/// <summary>
		/// �ش������� ���ڵ��� ã�Ƴ��� �н��ϴ�.
		/// </summary>
		/// <param name="filename">���ϸ�</param>
		/// <returns>�ؽ�Ʈ ����</returns>
		public static string ReadTextFile(string filename)
		{
			StreamReader sr = new StreamReader(filename,Get(filename));
			string temp = sr.ReadToEnd();
			sr.Close();
			return temp;
		}
		/// <summary>
		/// ���ڵ� �⺻���� �����մϴ�.
		/// </summary>
		/// <param name="enco">���ڵ���</param>
		public static void SetDefEncoding(System.Text.Encoding enco)
		{
			try
			{
				defaultEncoding = enco;
			}
			catch
			{
			}
		}
	}

}
