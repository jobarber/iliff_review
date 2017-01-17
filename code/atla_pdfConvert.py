'''
This module converts PDF files from ATLASerials into plain text files. An important part of
this process is striping out the ATLA statment at the end of the PDF. In addition PyPDF tends
to read an - as Š;
'''

import PyPDF2
import re
import os

in_path = '/Users/msaxton/PycharmProjects/iliff_review/pdfs/scanned/'
out_path = '/Users/msaxton/r_projects/iliff_review_project/iliff_corpus/'

class MyPageObject(PageObject):
    def extractText(self):
        """
        Locate all text drawing commands, in the order they are provided in the
        content stream, and extract the text.  This works well for some PDF
        files, but poorly for others, depending on the generator used.  This will
        be refined in the future.  Do not rely on the order of text coming out of
        this function, as it will change if this function is made more
        sophisticated.
        :return: a unicode string object.
        """
        text = u_("")
        content = self["/Contents"].getObject()
        if not isinstance(content, ContentStream):
            content = ContentStream(content, self.pdf)
        # Note: we check all strings are TextStringObjects.  ByteStringObjects
        # are strings where the byte->string encoding was unknown, so adding
        # them to the text here would be gibberish.
        for operands, operator in content.operations:
			print('operator =', operator)
			print('operator string =', end=' ')
            if operator == b_("Tj"):
				print('Tj')
                _text = operands[0]
                if isinstance(_text, TextStringObject):
                    text += _text
            elif operator == b_("T*"):
				print('T*')
                text += "\n"
            elif operator == b_("'"):
				print("'")
                text += "\n"
                _text = operands[0]
                if isinstance(_text, TextStringObject):
                    text += operands[0]
            elif operator == b_('"'):
				print('"')
                _text = operands[2]
                if isinstance(_text, TextStringObject):
                    text += "\n"
                    text += _text
            elif operator == b_("TJ"):
				print('TJ')
                for i in operands[0]:
                    if isinstance(i, TextStringObject):
                        text += i
                text += "\n"
        return text

def make_file_name(fileid):
    file_name = fileid[0:-4]
    return file_name

def clean_text(string_obj):
    clean_text = re.sub("Š", "-", string_obj)
    clean_text = re.sub("¿|¡|£|^|`|«", "", clean_text)
    clean_text = re.sub("Á|Â|Ã|Ä|Å|à|á|â|ã|ä|å", "a", clean_text)
    clean_text = re.sub("È|É|Ê|Ë|è|é|ê|ë", "e", clean_text)
    clean_text = re.sub("Ì|Í|Î|Ï|ì|í|î|ï", "i", clean_text)
    clean_text = re.sub("Ò|Ó|Ô|Õ|Ö|Ø|ò|ó|ô|õ|ö|ø", "o", clean_text)
    clean_text = re.sub("Ù|Ú|Û|Ü|ù|ú|û|ü", "u", clean_text)
    clean_text = re.sub("Ý|ý|ÿ", "y", clean_text)
    return clean_text

def ATLASpdf_to_text(out_path, file_name):
    pdf_pages = []
    pdfObj = open(in_path + fileid, 'rb')
    pdfReader = PyPDF2.PdfFileReader(pdfObj)
    page_length = pdfReader.numPages

    for i in range(0, (page_length-1)):
        pageObj = pdfReader.getPage(i)
        text = pageObj.extractText()
        pdf_pages.append(text)
    
    pdf_string = ' '.join(pdf_pages) #join with space not new line
    eol_string = "\r\n"

    pdf_string = pdf_string+eol_string # add EOL character to avoid later warnings

    final_string = clean_text(pdf_string)

    with open(out_path + file_name + '.txt', mode='w', encoding='utf-8') as f:
        f.write(final_string)
        
for fileid in os.listdir(in_path):
    if fileid.endswith('.pdf'):
        file_name = make_file_name(fileid)
        ATLASpdf_to_text(out_path, file_name)