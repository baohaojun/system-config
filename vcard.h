// -*- mode: c++ -*-
#ifndef _VCARD_H_
#define _VCARD_H_

struct VCard {
 public:
    QString mName;
    QStringList mTels;
    QStringList mEmails;
    QPixmap mAvatar;

    VCard(const VCard& vcard) :
        mName(vcard.mName),
        mTels(vcard.mTels),
        mEmails(vcard.mEmails) {};

    VCard() {};

};

#endif /* _VCARD_H_ */
