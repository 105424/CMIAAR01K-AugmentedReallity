# CMIAAR01K-AugmentedReallity
Repo containing my homework for CMIAAR01K Augmented Reality: maak de Rotterdamse haven zichtbaar

## Install
The assignment are made as seperate stack applications so the reccommend way to run them is trough [stack](http://docs.haskellstack.org/en/stable/README.html)

```
mv assignment_1
stack install
stack build
stack exec assignment1-exe "test.jpg"
```

### Assignment1
For the first assignment we needed to implement a BW filter for both the RGB as the YUV format.
Running the following command will create three images from the test.jpg image.
```
mv assignment_1
stack install
stack build
stack exec assignment1-exe "test.jpg"
```
* The `test.jpgbwFilterWithRGB` is the RGB based BW
* The `test.jpgfromRGBToYUVToRGB` is the RGB conversion to YUV and back to RGB
* the `test.filterjpgbwFilterWithYUV` is the conversion from RGB to YUV and back after the YUV based BW filter is applied