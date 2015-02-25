// -*- mode: c++ -*-
#ifndef _VCARD_H_
#define _VCARD_H_

struct VCard {
 public:
    QString mName;
    QStringList mTels;
    QStringList mEmails;
    QPixmap mAvatar;
    mutable QStringList mPinyin;

    VCard(const VCard& vcard) :
        mName(vcard.mName),
        mTels(vcard.mTels),
        mEmails(vcard.mEmails),
        mAvatar(vcard.mAvatar),
        mPinyin(vcard.mPinyin)
    {};

    bool operator==(const VCard& other) const {
        return mName == other.mName &&
            mTels == other.mTels &&
            mEmails == other.mEmails;
    };

    VCard() {};

};

#endif /* _VCARD_H_ */
