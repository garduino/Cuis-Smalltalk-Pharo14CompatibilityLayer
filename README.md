Cuis-Pharo14CompatibilityLayer
==============================

Cuis Pharo 1.4 Compatibility Layer.

Different packages, needed for install Pharo 1.4 packages, in differents degrees of completion.

The install order and needs are described in the corresponding master package, I mean, for example, if you want to install
Cuis-JSON from its repository, in its README you will find the instructions for install packages of the Pharo14CompatibilityLayer
repo.

The source of the ports is Pharo 1.4 Summer.

Notes about specific packages:

Cuis-Network-MIME: Read MIMEDocument class comment.


Cuis-NetworkTest-Url: In the Pharo package most of classes inherit from ClassTestCase, I modified here to inherit from TestCase.