from textblob import TextBlob

#Read and open raw text file
text_file = open("<FILENAME>", "r").read()
content = text_file.decode('utf-8')
#print content

#First translation
text_input = TextBlob(content)
text_output = text_input.translate(to='ko')
print text_output

#Second translation
def replace_all(text, dic):
    for i, j in dic.iteritems():
        text = text.replace(i, j)
    return text 

#Dictionaries, WIP
dic_formal = {
    u"안녕하세요": u"안녕하십니까",
    u"내 이름은" : u"제 이름은",
    u"환영" : u"환영합니다"
}

dic_informal = {
    u"안녕하세요": u"안녕",
    u"리처드입니다" : u"리처드야",
    u"환영" : u"환영해"
}

#Environment Preferences
def pref_environment(env):
    if env == "Business" or env == "Funeral":
        return replace_all(text_output, dic_formal)
    elif env == "Social" or env == "School":
        return replace_all(text_output, dic_informal)
    else:
        print "Please select your Environment settings."
environment = raw_input()
print pref_environment(environment)

#Audience Preference
def pref_audience(aud):
    if aud == "Older Person" or auda == "Professional":
        return replace_all(text_output, dic_formal)
    elif aud == "Younger Person" or aud == "Friend":
        return replace_all(text_output, dic_informal)
    else:
        print "Please select your Audience settings."
audience = raw_input()
print pref_audience(audience)
