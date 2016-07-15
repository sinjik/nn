// NeuralNetwork.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include<algorithm>
#include<cassert>
#include<iomanip>
#include<iostream>
#include<memory>
#include<random>
#include<vector>

class DimensionMismatchException : public std::exception
{
public:
	DimensionMismatchException() {}
};

template<typename T>
class Matrix {
public:
	Matrix(unsigned nrows, unsigned ncols)
		: nrows_(nrows)
		, ncols_(ncols)
		, nodeep_(false)
		, mempool_(new T [nrows*ncols*sizeof(T)])
		, data_(mempool_.get()) {}

	~Matrix() {
		data_ = nullptr;
		mempool_.reset();
	}

	Matrix(const Matrix<T> &mat, bool nodeep=false) 
	: nrows_(0)
	, ncols_(0)
	, nodeep_(nodeep)
	, mempool_(nullptr)
	, data_(nullptr) {
		// deep copy
		*this = mat;
	}

	// deep copy assignment
	Matrix<T>& operator=(const Matrix<T>& mat) {
		nrows_ = mat.nrows();
		ncols_ = mat.ncols();
		if (nodeep_) {
			mempool_ = mat.mempool_;
		} else {
			mempool_.reset(new T[mat.nrows() * mat.ncols() * sizeof(T)]);
			memcpy(mempool_.get(), mat.mempool_.get(), mat.size());
		}
		data_ = mempool_.get();
		return *this;
	}

	// move assignment
	Matrix<T>& operator=(Matrix<T>&& mat) {
		if (this != &mat) {
			nrows_ = mat.nrows();
			ncols_ = mat.ncols();
			mempool_ = mat.mempool_;
			data_ = mat.data_;
			mat.mempool_.reset();
			mat.data_ = nullptr;
		}
		return *this;
	}

	inline T& operator () (unsigned r, unsigned c) {
		return data_[r * ncols_ + c];
	}

	inline const T& operator () (unsigned r, unsigned c) const {
		return data_[r * ncols_ + c];
	}

	// tranposed.
	Matrix<T> Tr() const {
		if (nrows() == 1
			&& nodeep_ == true) {
			Matrix<T> t(*this, true /* nodeep */);
			std::swap(t.nrows_, t.ncols_);
			return t;
		}
		Matrix<T> t(ncols(), nrows());
		for (unsigned r = 0; r < nrows(); ++r) {
			for (unsigned c = 0; c < ncols(); ++c) {
				t(c, r) = (*this)(r, c);
			}
		}
		return t;
	}

	Matrix<T> row(unsigned r) const {
		if ( r >= nrows() )
			throw DimensionMismatchException();
		Matrix<T> rs(*this, true /* nodeep */);
		rs.data_ = &(this->data_[r * ncols_]);
		rs.nrows_ = 1;
		return rs;
	}

#if 0
	Matrix<T> rows(unsigned start, unsigned end) const {
		if ( start > end 
			 || start >= nrows()
			 || end >= nrows() )
			throw DimensionMismatchException();
		Matrix<T> rs(end-start+1, ncols());
		memcpy(rs.data_, data_ + start * ncols(), rs.size());
		return rs;
	}
#endif

	Matrix<T> cols(unsigned start, unsigned end) {
		if (start > end
			|| start >= ncols()
			|| end >= ncols())
			throw DimensionMismatchException();
		Matrix<T> cl(nrows(), end - start + 1);
		for (unsigned r = 0; r < cl.nrows(); ++r) {
			for (unsigned c = 0; c < cl.ncols(); ++c) {
				cl(r, c) = (*this)(r, c+start);
			}
		}
		return cl;
	}

	Matrix<T> operator+(const Matrix<T> &mat) {
		if (ncols() != mat.ncols() || nrows() < mat.nrows())
			throw DimensionMismatchException();
		Matrix<T> added(nrows(), ncols());
		for (unsigned r = 0; r < added.nrows(); ++r) {
			for (unsigned c = 0; c < added.ncols(); ++c) {
				added(r, c) = (*this)(r, c) + mat(r%mat.nrows(), c);
			}
		}
		return added;
	}

	Matrix<T> &operator+=(const Matrix<T> &mat) {
		if (ncols() != mat.ncols() || nrows() != mat.nrows())
			throw DimensionMismatchException();
		for (unsigned r = 0; r < nrows(); ++r) {
			for (unsigned c = 0; c < ncols(); ++c) {
				(*this)(r, c) += mat(r, c);
			}
		}
		return (*this);
	}

	Matrix<T> operator-(const Matrix<T> &mat) {
		if (ncols() != mat.ncols() || nrows() < mat.nrows())
			throw DimensionMismatchException();
		Matrix<T> added(nrows(), ncols());
		for (unsigned r = 0; r < added.nrows(); ++r) {
			for (unsigned c = 0; c < added.ncols(); ++c) {
				added(r, c) = (*this)(r, c) - mat(r%mat.nrows(), c);
			}
		}
		return added;
	}

	Matrix<T> &operator-=(const Matrix<T> &mat) {
		if (ncols() != mat.ncols() || nrows() != mat.nrows())
			throw DimensionMismatchException();
		for (unsigned r = 0; r < nrows(); ++r) {
			for (unsigned c = 0; c < ncols(); ++c) {
				(*this)(r, c) -= mat(r, c);
			}
		}
		return (*this);
	}

	Matrix<T> operator-(float m) {
		Matrix<T> added(nrows(), ncols());
		for (unsigned r = 0; r < multed.nrows(); ++r) {
			for (unsigned c = 0; c < multed.ncols(); ++c) {
				added(r, c) = (*this)(r, c) - m;
			}
		}
		return added;
	}

	Matrix<T> operator*(const Matrix<T> &mat) {
		if (ncols() != mat.ncols() || nrows() != mat.nrows())
			throw DimensionMismatchException();
		Matrix<T> multed(nrows(), ncols());
		for (unsigned r = 0; r < multed.nrows(); ++r) {
			for (unsigned c = 0; c < multed.ncols(); ++c) {
				multed(r, c) = (*this)(r, c) * mat(r, c);
			}
		}
		return multed;
	}

	Matrix<T> operator*(float m) {
		Matrix<T> multed(nrows(), ncols());
		for (unsigned r = 0; r < multed.nrows(); ++r) {
			for (unsigned c = 0; c < multed.ncols(); ++c) {
				multed(r, c) = (*this)(r, c) * m;
			}
		}
		return multed;
	}

	Matrix<T> &operator*=(float m) {
		for (unsigned r = 0; r < nrows(); ++r) {
			for (unsigned c = 0; c < ncols(); ++c) {
				(*this)(r, c) *= m;
			}
		}
		return (*this);
	}


	Matrix<T> dot(const Matrix<T> &mat) {
		if (ncols() != mat.nrows())
			throw DimensionMismatchException();

		Matrix<T> dotted(nrows(), mat.ncols());
		for (unsigned r = 0; r < dotted.nrows(); ++r) {
			for (unsigned c = 0; c < dotted.ncols(); ++c) {
				T sum = 0;
				for (unsigned k = 0; k < ncols_; ++k) {
					sum += (*this)(r, k) * mat(k, c);
				}
				dotted(r, c) = sum;
			}
		}
		return dotted;
	}

	unsigned matchcount(const Matrix<T> &mat) const {
		unsigned tot = 0;
		for (unsigned r = 0; r < nrows(); ++r) {
			for (unsigned c = 0; c < ncols(); ++c) {
				if ((*this)(r, c) == mat(r, c))
					++tot;
			}
		}
		return tot;
	}

	T sum() const {
		T s = 0;
		for (unsigned r = 0; r < nrows(); ++r) {
			for (unsigned c = 0; c < ncols(); ++c) {
				s += (*this)(r, c);
			}
		}
		return s;
	}

	void zeros() {
		memset(data_, 0x0, nrows()*ncols()*sizeof(T));
	}

	inline unsigned nrows() const { return nrows_;  }
	inline unsigned ncols() const { return ncols_; }
	inline unsigned size() const { return nrows()*ncols()*sizeof(T); }

	inline std::string shape() const {
		return std::to_string(nrows())
			+ std::string("x") 
			+ std::to_string(ncols());
	}

	void dump(const std::string &header) {
		std::cout << header.c_str() << std::endl;
		for (unsigned r = 0; r < ncols_; ++r) {
			for (unsigned c = 0; c < nrows_; ++c) {
				std::cout << std::setw(7) << std::setprecision(2) << (*this)(r, c);
			}
			std::cout << std::endl;
		}
		std::cout << std::endl;
	}
private:
	Matrix() = delete;

	//friend void Mnist::load_data(const std::string &path, Matrix<float> &img);
	friend class Mnist;

	unsigned nrows_;
	unsigned ncols_;
	bool nodeep_;
	std::shared_ptr<T> mempool_;
	// data_ points to a location in mempool_.
	T* data_;
};

template<typename T>
Matrix<T> operator*(float lhs, Matrix<T> &rhs) {
	return rhs * lhs;
}

template<typename T>
void random(Matrix<T> &mat) {
	std::random_device rd;
	std::mt19937 mt(rd());
	std::uniform_int_distribution<T> dist(0, 9);

	for (unsigned r = 0; r < mat.nrows(); ++r) {
		for (unsigned c = 0; c < mat.ncols(); ++c) {
			mat(r, c) = dist(mt);
		}
	}
}

template<>
void random(Matrix<float> &mat) {
	std::random_device rd;
	std::mt19937 mt(rd());
	std::uniform_real_distribution<float> dist(0, 1.0);

	for (unsigned r = 0; r < mat.nrows(); ++r) {
		for (unsigned c = 0; c < mat.ncols(); ++c) {
			mat(r, c) = dist(mt);
		}
	}
}

template<>
void random(Matrix<unsigned char> &mat) {
	std::random_device rd;
	std::mt19937 mt(rd());
	std::uniform_int_distribution<int> dist(0, 10);

	for (unsigned r = 0; r < mat.nrows(); ++r) {
		for (unsigned c = 0; c < mat.ncols(); ++c) {
			mat(r, c) = (unsigned char)dist(mt);
		}
	}
}

#include<cmath>
#include<list>

template<typename T>
class Network {
public:
	Network(const std::vector<unsigned>& sizes) throw()
		: sizes_(sizes)
		, biases_()
		, weights_() {

		std::cout << "sizes: ";
		for (auto i : sizes)
			std::cout << i << " ";
		std::cout << std::endl;

		for (std::vector<unsigned>::iterator y = sizes_.begin(); y != sizes_.end(); ++y) {
			Matrix<float> mat(*y, 1);
			random(mat);
			biases_.push_back(mat);
			std::cout << "biases=" << 1 << "x" << *y << std::endl;
		}
		{
			// dummy for input
			Matrix<float> mat(sizes_[0], 1);
			weights_.push_back(mat);
		}
		for (std::vector<unsigned>::iterator y = sizes_.begin() + 1; y != sizes_.end(); ++y) {
			Matrix<float> mat(*y, *(y-1));
			random(mat);
			weights_.push_back(mat);
			std::cout << "weights=" << *(y-1) << "x" << *y << std::endl;
		}
	}

	virtual ~Network(){
	}


	void SGD(Matrix<T> &training_data, Matrix<T> &labels, 
		unsigned epochs, unsigned mini_batch_size, float eta) {
		unsigned n = training_data.nrows();
		std::srand(unsigned(std::time(0)));
		std::vector<unsigned> index(n);
		for (unsigned e = 0; e < epochs; ++e) {
			std::random_shuffle(index.begin(), index.end());
			// training_data
			for (unsigned k = 0; k < n; k += mini_batch_size) {
				unsigned mb_size = (n - k) > mini_batch_size ? mini_batch_size : (n - k);
				update_mini_batch(training_data, labels, index, k, mb_size, eta);
			}
			std::cout << "epoch: " << e << std::endl;
		}
	}

	void update_mini_batch
	(const Matrix<T>& mb_x, const Matrix<T>& mb_y, 
		const std::vector<unsigned> &index, unsigned k, 
		unsigned mb_size, float eta) {
		std::vector<Matrix<T>> nabla_b = zeros(biases_);
		std::vector<Matrix<T>> nabla_w = zeros(weights_);
		for (unsigned i = 0; i < mb_size; ++i) {
			std::pair<std::vector<Matrix<T>>, std::vector<Matrix<T>>> delta_nabla =
				backprop(mb_x.row(index[k+i]), mb_y.row(index[k+i]));
			for (unsigned j = 1; j < weights_.size(); ++j) {
				nabla_b[j] += delta_nabla.first[j];
				nabla_w[j] += delta_nabla.second[j];
			}
			float learning_rate = (eta / mb_size);
			// skip input layer
			for (unsigned j = 1; j < weights_.size(); ++j) {
				weights_[j] -= (nabla_w[j] * learning_rate);
				biases_[j] -= (nabla_b[j] * learning_rate);
			}
		}
	}

	// Return a tuple (nabla_b, nabla_w) representing the
	// gradient for the cost function Cx. *nabla_b* and
	// *nabla_w* are layer - by -layer lists of Matrix,
	// similar to biases_ and weights_.
	std::pair<std::vector<Matrix<T>>, std::vector<Matrix<T>>> 
		backprop(const Matrix<T>& feature, const Matrix<T>& label) {
		std::vector<Matrix<T>> nabla_b = zeros(biases_);
		std::vector<Matrix<T>> nabla_w = zeros(weights_);
		//
		// feed forward
		//
		Matrix<T> activation = feature.Tr();
		// store all layer's activations
		std::vector<Matrix<T>> activations;
		activations.push_back(activation);
		// store all z vectors
		// z = w.a + b
		std::vector<Matrix<T>> zs;
		zs.push_back(feature);
		for (unsigned i = 1; i < weights_.size(); ++i) {
			Matrix<T> z = weights_[i].dot(activation) + biases_[i];
			zs.push_back(z);
			activation = sigmoid(z);
			activations.push_back(activation);
		}
		//
		// backward
		//

		// output layer, L
		Matrix<T> delta = cost_derivative(activations.back(), label) * sigmoid_prime(zs.back());
		nabla_b.back() = delta;
		nabla_w.back() = delta.dot(activations[activations.size() - 2].Tr());
		// hidden layers
		// L-1, L-2, ..., 2
		for (size_t layer = sizes_.size() - 2; layer > 0; --layer) {
			Matrix<T> z = zs[layer];
			Matrix<T> sp = sigmoid_prime(z);
			delta = sp * weights_[layer + 1].Tr().dot(delta);
			nabla_b[layer] = delta;
			nabla_w[layer] = delta.dot(activations[layer - 1].Tr());
		}
		return std::make_pair(nabla_b, nabla_w);
	}

	float evaluate(const Matrix<T> &testImages, const Matrix<T> &testLabels) {
		unsigned sum = 0;
		for (unsigned r = 0; r < testImages.nrows(); ++r) {
			Matrix<T> row = testImages.row(r);
			Matrix<T> testPredicted = feedForward(row);
			sum += testPredicted.matchcount(testLabels);
		}
		return sum / float(testImages.nrows()*testLabels.nrows());
	}

private:
	Network() = delete;

	Matrix<T> feedForward(const Matrix<T> &input) {
		assert(input.nrows() == 1);
		Matrix<T> features = input.Tr();
		size_t len = weights_.size();
		// skip input layer
		for (size_t idx = 1; idx < len; ++idx) {
#if 0
			std::cout << weights_[idx].shape()
				<< ".dot("
				<< features.shape()
				<< ") + "
				<< biases_[idx].shape()
				<< std::endl;
#endif
			features = weights_[idx].dot(features) + biases_[idx];
		}

		for (unsigned r = 0; r < features.nrows(); ++r) {
			for (unsigned c = 0; c < features.ncols(); ++c) {
				features(r, c) = (T)sigmoid(features(r, c));
			}
		}
		//std::cout << "feedForward:" << features.shape() << std::endl;
		return features;
	}

	Matrix<T> sigmoid(Matrix<T> &mat) const {
		for (unsigned r = 0; r < mat.nrows(); ++r) {
			for (unsigned c = 0; c < mat.ncols(); ++c) {
				mat(r, c) = (T)sigmoid(mat(r, c));
			}
		}
		return mat;
	}

	inline float sigmoid(float z) const {
		return float(1.0 / (1.0 + exp(-1.0*z)));
	}

	inline float sigmoid_prime(float z) const {
		return sigmoid(z) * (1.0 - sigmoid(z));
	}

	Matrix<T> sigmoid_prime(Matrix<T> &mat) const {
		Matrix<T> prime(mat);
		for (unsigned r = 0; r < mat.nrows(); ++r) {
			for (unsigned c = 0; c < mat.ncols(); ++c) {
				float z = mat(r, c);
				mat(r, c) = sigmoid(z) * (1.0f - sigmoid(z));
			}
		}
		return prime;
	}

	Matrix<T> cost_derivative(Matrix<T> &output_activation, const Matrix<T> &y) {
		return output_activation - y;
	}

	std::vector<Matrix<float>> zeros(std::vector<Matrix<float>>& input) {
		std::vector<Matrix<float>> zs;
		for (auto i : input) zs.push_back(i);
		for (auto z : zs) z.zeros();
		return zs;
	}

#if 0
	std::vector<Matrix<float>> sums(std::vector<Matrix<float>>& x, std::vector<Matrix<float>>& y) {
		assert(x.size() == y.size());
		std::vector<Matrix<float>> s;
		for (unsigned i = 1; i < x.size(); ++i) {
			s.push_back(x[i] + y[i]);
		}
		return s;
	}
#endif

	std::vector<unsigned> sizes_;
	std::vector<Matrix<float>> biases_;
	std::vector<Matrix<float>> weights_;
};

#include <fstream>
#include <iostream>
#include <string>

class Mnist {
public:
	Mnist() throw()
		: trainImages_(60000, 28*28) 
		, trainLabels_(60000, 1)
		, testImages_(10000, 28 * 28)
		, testLabels_(10000, 1)
	{
		load_data(train_images, trainImages_);
		load_data(train_labels, trainLabels_);
		load_data(test_images, testImages_);
		load_data(test_labels, testLabels_);
	}

	inline const unsigned imgSize() const { return 28 * 28; }

	inline Matrix<float> &trainImages() { return trainImages_; }
	inline Matrix<float> &trainLabels() { return trainLabels_; }
	inline Matrix<float> &testImages() { return testImages_; }
	inline Matrix<float> &testLabels() { return testLabels_; }

private:
	void load_data(const std::string &path, Matrix<float> &img) {
		std::ifstream ifs(path, std::ifstream::in | std::ifstream::binary);
		if (path.compare(train_images)==0 || path.compare(test_images)==0)
			ifs.seekg(16);
		else if (path.compare(train_labels)==0 || path.compare(test_labels)==0)
			ifs.seekg(8);
		unsigned sz = img.nrows()*img.ncols();
		std::unique_ptr<unsigned char []> data(new unsigned char [sz]);
		ifs.read((char *)data.get(), img.nrows()*img.ncols());
		ifs.close();
		for (unsigned i = 0; i < sz; ++i)
			img.data_[i] = float(data[i]);
	}

	//
	// TRAINING SET LABEL FILE(train - labels - idx1 - ubyte) :
	//
	//	[offset] [type]          [value]          [description]
	//	0000     32 bit integer  0x00000801(2049) magic number(MSB first)
	//	0004     32 bit integer  60000            number of items
	//	0008     unsigned byte   ??               label
	//
	// TRAINING SET IMAGE FILE(train - images - idx3 - ubyte) :
	//
	//	[offset] [type]          [value]          [description]
	//	0000     32 bit integer  0x00000803(2051) magic number
	//	0004     32 bit integer  60000            number of images
	//	0008     32 bit integer  28               number of rows
	//	0012     32 bit integer  28               number of columns
	//	0016     unsigned byte	 ??				  pixel
	//
	//
	//
	// TEST SET LABEL FILE(t10k - labels - idx1 - ubyte) :
	//
	//	[offset] [type]          [value]          [description]
	//	0000     32 bit integer  0x00000801(2049) magic number(MSB first)
	//	0004     32 bit integer  10000            number of items
	//	0008     unsigned byte	 ??				  label
	//	0009     unsigned byte	 ??				  label
	//	........
	//	xxxx     unsigned byte	 ??				  label
	//	The labels values are 0 to 9.
	//
	//	TEST SET IMAGE FILE(t10k - images - idx3 - ubyte) :
	//
	//	[offset] [type]          [value]          [description]
	//	0000     32 bit integer  0x00000803(2051) magic number
	//	0004     32 bit integer  10000            number of images
	//	0008     32 bit integer  28               number of rows
	//	0012     32 bit integer  28               number of columns
	//	0016     unsigned byte	 ??				  pixel
	//	0017     unsigned byte	 ??				  pixel
	//	........
	//	xxxx     unsigned byte	 ??				  pixel
	//	Pixels are organized row - wise.Pixel values are 0 to 255. 0 means background(white), 255 means foreground(black).
	//

	const std::string train_images = "c:\\temp\\train-images";
	const std::string train_labels = "c:\\temp\\train-labels";
	const std::string test_images = "c:\\temp\\t10k-images";
	const std::string test_labels = "c:\\temp\\t10k-labels";

	Matrix<float> trainImages_;
	Matrix<float> trainLabels_;
	Matrix<float> testImages_;
	Matrix<float> testLabels_;
};

#include <initializer_list>

int main()
{
	Matrix<float> m1(3, 3);
	Matrix<float> m2(3, 3);

	random(m1);
	random(m2);

	m1.dump("m1");
	m2.dump("m2");

	Matrix<float> m3 = m1.dot(m2);
	m3.dump("m3");

	Matrix<float> m4 = m1 + m2;
	m4.dump("m4");

	Matrix<float> m5 = m1 * 1.0;
	m5.dump("m5");

	Matrix<float> m6 = m1 * 2.0;
	m6.dump("m6");

	Mnist nist;
	Matrix<float> mm1(28*28, 1);
	random(mm1);

	Matrix<float> mm2
		= nist.trainImages().dot(mm1);

	std::cout << "trainImage=" << nist.trainImages().shape() << std::endl;
	std::cout << "mm1=" << mm1.shape() << std::endl;
	std::cout << "mm2=" << mm2.shape() << std::endl;

	std::vector<unsigned> sizes({ nist.imgSize(), 30, 10 });

	std::unique_ptr<Network<float>> network 
		= std::make_unique<Network<float>>(sizes);

	std::cout << "trainImages = " << nist.trainImages().shape() << std::endl;

	const unsigned epoch = 30;
	const unsigned mini_batch_size = 10;
	const float eta = 3.0f;
	
	network->SGD(nist.trainImages(), nist.trainLabels(), epoch, mini_batch_size, eta);

	std::cout << "testImages = " << nist.testImages().shape() << std::endl;
	std::cout << std::endl;

	float score = 
		network->evaluate(nist.testImages(), nist.testLabels());

	std::cout << "Score = " << std::setprecision(2) << score*100. << "%" << std::endl;
	std::cout << std::endl;

	return 0;
}

